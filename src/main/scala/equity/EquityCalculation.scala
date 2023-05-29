package equity

import cats._
import cats.syntax.all._
import cats.data.{State}
import cats.effect.IO
import cats.effect.std.Random
import cats.implicits.catsSyntaxApplicativeId
import poker.OrderInstances.handOrder
import poker.{Card, Hand, Position}
import sim.{Deck, Player, Result, Setup}

// TODO Get rid of the Sim prefix, put this in a sim package!

// TODO Name: Positional Equities?
final case class EquityCalculation(equities: Map[Position, List[Double]])

sealed trait EquityService[F[_]] {

  type SimState[A] = State[(Setup, Deck), A]

  type DeckState[A] = State[Deck, A]

//  def deckFrom: SimSetup[G] => SimDeck => SimDeck
// TODO These other functions are really implementation, they
  // don't belong in the algebra
//  def hydratedSim: SimState[EquityCalculation]

//  def runThis(setup: Setup)(deck: Deck): EquityCalculation

//  def equityFrom(result: Result): EquityCalculation

//  def combinedEquity(
//    eq1: EquityCalculation,
//    eq2: EquityCalculation
//  ): EquityCalculation

  def theFinalEquityOf[F[_]: Functor: Random: Applicative](
    sim: Setup,
    n: Int
  )(deck: Deck): F[EquityCalculation]

}

object EquityService extends EquityService[IO] {

  val dealOneCard: DeckState[Card] = State[Deck, Card] {
    _.cards match {
      case card :: remaining => (Deck(remaining), card)
      case _                 => throw new Exception("BUG: No Cards in Deck!!!")
    }
  }

  val getOrDeal: Option[Card] => DeckState[Card] = optCard =>
    for {
      newCard <- optCard match {
        case Some(card) => State.pure[Deck, Card](card)
        case None       => dealOneCard
      }
    } yield newCard

  def deckFrom: Setup => Deck => Deck = {

    def allCardsFrom(setup: Setup): List[Card] = {
      val boardCards = setup match {
        case Setup(_, c1, c2, c3, t, r) =>
          List(c1, c2, c3, t, r).collect { case Some(x) => x }
      }

      // Player cards
      val cardsFrom: Player => List[Card] = { case Player(_, c1, c2) =>
        List(c1, c2).collect { case Some(x) => x }
      }

      def allPlayerCards = setup.players.foldLeft(List.empty[Card]) {
        (cards, player) =>
          cards ++ cardsFrom(player)
      }

      boardCards ++ allPlayerCards
    }

    sim: Setup =>
      deck: Deck =>
        val finalDeck = deck.cards
          .filterNot(allCardsFrom(sim).contains(_))
          .pure[Id]
        Deck(finalDeck)
  }

  def winners(l: List[(Position, Hand)]): Result =
    Result(
      l.maximumByList[Hand](x => x._2)
        .map { case (playerPosition, _) => playerPosition }
    )

  def preFlopEquity(setup: Setup): EquityCalculation = {
    val result: Result = winners(
      PreFlop
        .allHands(setup)
    )
    equityFrom(result)
  }

  private def hydratePlayer(player: Player)(
    deck: Deck
  ): (Player, Deck) =
    Player.applyK(player.position, player.card1, player.card2)(deck)

  private val stateAfterPreFlopCards: SimState[EquityCalculation] =
    State[(Setup, Deck), EquityCalculation] { case (sim, deck) =>
      val (allNewPlayers, finalDeck) = sim.players.foldLeft(
        (List.empty[Player], deck)
      ) { case ((allPlayers, deck), player) =>
        val (newPlayer, newDeck) = hydratePlayer(player)(deck)
        (allPlayers :+ newPlayer, newDeck)
      }

      val newSim = sim.copy(players = allNewPlayers)

      (
        (newSim, finalDeck),
        preFlopEquity(newSim)
      )
    }

  def after(state: SimBoardState): SimState[EquityCalculation] =
    State[(Setup, Deck), EquityCalculation] { case (setup, deck) =>
      val (newDeck, newCard) = state match {
        // TODO this is wonky but kinda acts as a "burn" card...
        // Must change this.
        case PreFlop   => getOrDeal(Option.empty[Card]).run(deck).value
        case FlopCard1 => getOrDeal(setup.card1).run(deck).value
        case FlopCard2 => getOrDeal(setup.card2).run(deck).value
        case FlopCard3 => getOrDeal(setup.card3).run(deck).value
        case Turn      => getOrDeal(setup.turn).run(deck).value
        case River     => getOrDeal(setup.river).run(deck).value
      }

      val newSim = state match {
        case PreFlop   => setup
        case FlopCard1 => setup.copy(card1 = newCard.pure[Option])
        case FlopCard2 => setup.copy(card2 = newCard.pure[Option])
        case FlopCard3 => setup.copy(card3 = newCard.pure[Option])
        case Turn      => setup.copy(turn = newCard.pure[Option])
        case River     => setup.copy(river = newCard.pure[Option])
      }

      val equityCalculation = equityFrom(winners(state.allHands(newSim)))

      ((newSim, newDeck), equityCalculation)
    }

  // TODO These after calls can actually be distinct functions.
  // TODO I don't really need my state's result to be EquityCalculation.  It can be Unit.
  def hydratedSim: SimState[EquityCalculation] =
    for {
      _           <- stateAfterPreFlopCards
      _           <- after(FlopCard1)
      _           <- after(FlopCard2)
      _           <- after(FlopCard3)
      _           <- after(Turn)
      finalEquity <- after(River)
    } yield finalEquity

  def runThis(
    setup: Setup
  )(deck: Deck): EquityCalculation = {
    val newDeck = deckFrom(setup)(deck)
    hydratedSim.runA(setup, newDeck).value
  }

  def equityFrom(result: Result): EquityCalculation = {
    val divisor = result.winnersList.size
    EquityCalculation(result.winnersList.map(_ -> List(100.0 / divisor)).toMap)
  }

  def combinedEquity(
    eq1: EquityCalculation,
    eq2: EquityCalculation
  ): EquityCalculation =
    EquityCalculation(
      eq1.equities ++ eq2.equities
        .map { case (k, v) =>
          k -> (v ++ eq1.equities.getOrElse(k, List.empty))
        }
    )
  def aggregateEquityFromMany(
    results: List[EquityCalculation]
  ): EquityCalculation =
    results
      .foldLeft(
        EquityCalculation(Map.empty[Position, List[Double]])
      ) { (s, v) =>
        combinedEquity(s, v)
      }

  override def theFinalEquityOf[F[_]: Functor: Random: Applicative](
    sim: Setup,
    n: Int
  )(deck: Deck): F[EquityCalculation] =
    // TODO The Unit result means I did NOT short circuit
    for {
//      _             <- SimSetup.validate(sim)
      shuffledDecks <- (1 to n).toList.traverse(_ => deck.shuffle[F])
      simResults = shuffledDecks.map(runThis(sim))
    } yield aggregateEquityFromMany(simResults)
}

// TODO Organization - what should be in Equity Service vs SimBoardState
sealed trait SimBoardState {
  def allHands(sim: Setup): List[(Position, Hand)] = {
    val boardCards: List[Card] = sim match {
      case Setup(_, card1, card2, card3, turn, river) =>
        List(card1, card2, card3, turn, river).collect { case Some(x) => x }
    }

    // TODO If this is safe I must fix the Hand Ranking function .
    sim.players.map { case Player(position, card1, card2) =>
      (
        position,
        Hand.rank(List(card1, card2).collect { case Some(x) =>
          x
        } ++ boardCards)
      )
    }
  }
}

object SimBoardState
case object PreFlop   extends SimBoardState
case object FlopCard1 extends SimBoardState
case object FlopCard2 extends SimBoardState
case object FlopCard3 extends SimBoardState
case object Turn      extends SimBoardState
case object River     extends SimBoardState
