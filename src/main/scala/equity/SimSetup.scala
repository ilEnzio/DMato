package equity

import cats.Id
import cats.effect.IO
import cats.effect.std.Random
import cats.implicits.catsSyntaxApplicativeId
import org.scalactic.anyvals.NonEmptySet
import poker.Deck.{startingDeck, StartingDeck}
import poker.Street.River
import poker.{Card, Position, Rank, ShowDown, Suit}

final case class SimSetup[F[_]] private (
  // Do I even need a deck? The deck basically get derived
  // from the the cards that are prepopulated by the sim
  deck: SimDeck, // TODO I might have to type this later...
  players: List[SimPlayer[F]],
  card1: F[Card],
  card2: F[Card],
  card3: F[Card],
  turn: F[Card],
  river: F[Card]
) {

  def mapK[G[_]](f: F[Card] => G[Card]): SimSetup[G] =
    SimSetup(
      deck,
      players,
      f(card1),
      f(card2),
      f(card3),
      f(turn),
      f(river)
    )

}
object SimSetup {
  def applyK[F[_]: Random, Applicative](
    players: List[SimPlayer[F]],
    card1: F[Card],
    card2: F[Card],
    card3: F[Card],
    turn: F[Card],
    river: F[Card]
  ): SimSetup[F[_]] =
    ???

}

final case class SimPlayer[F[_]]( // TODO I think this is ??Option??
  position: Position,
  card1: F[Card],
  card2: F[Card]
) {
  // TODO I think my F is Option and my G is IO as I have to randomly
  // generate it.
  def mapK[G[_]](f: F[Card] => G[Card]): SimPlayer[G] =
    SimPlayer(
      position,
      f(card1),
      f(card2)
    )
}

object SimPlayer {
  def applyK[F[_]](
    position: Position,
    card1: F[Card],
    card2: F[Card]
  ): SimPlayer[F[_]] = ???
}

final case class SimDeck(cards: List[Card])

final case class SimResult()

sealed trait EquityService[F[_], G[_]] {

  // TODO These are the two things you need to turn a set up
  // into a River board state.

  // Refactor out IO and Option
  def deckFrom: SimSetup[Option] => G[SimDeck] // Do
  def hydratedSim(sim: SimSetup[Option], deck: G[SimDeck]): SimSetup[Option]

  def riverFrom: SimSetup[Option] => River
  def equity: List[River] => SimResult

  def runSim(simSetupOP: SimSetup[Option]): Option[NonEmptySet[Position]] = {
    val deck    = deckFrom(simSetupOP)
    val setUpID = hydratedSim(simSetupOP, deck)
    val river   = riverFrom(setUpID)
    ShowDown.fromRiver(river)
  }

  def allSimResults(
    sim: SimSetup[Option],
    n: Int
  ): List[Option[NonEmptySet[Position]]] =
    (1 to n).toList.map(_ => runSim(sim))
}

object EquityService extends EquityService[Option, IO] {

  def getOrDeal(maybe: Option[Card], deck: SimDeck): (Card, SimDeck) =
    maybe.fold {
      // TODO not safe
      (deck.cards.head, SimDeck(deck.cards.tail))
    }(x => (x, deck))

  override def deckFrom: SimSetup[Option] => IO[SimDeck] = ???

  override def hydratedSim(
    sim: SimSetup[Option],
    deck: IO[SimDeck]
  ): SimSetup[Option] = {

    // Hydrate Flop
    val (newCard1, newDeck)  = getOrDeal(sim.card1, sim.deck)
    val part1                = sim.copy(card1 = newCard1[Id], deck = newDeck[Id])
    val (newCard2, newDeck2) = getOrDeal(part1.card2, part1.deck)
    val part2                = part1.copy(card2 = newCard2[Id], deck = newDeck2[Id])
    val (newCard3, newDeck3) = getOrDeal(part2.card3, part2.deck)
    val part3                = part2.copy(card3 = newCard3[Id], deck = newDeck3[Id])

    // Hydrate Turn and River
    val (newTurn, newDeck4)  = getOrDeal(part3.turn, part3.deck)
    val part4                = part3.copy(turn = newTurn[Id], deck = newDeck4[Id])
    val (newRiver, newDeck5) = getOrDeal(part4.card3, part4.deck)
    val part5                = part2.copy(river = newRiver[Id], deck = newDeck5[Id])

    part5
    // TODO: The players haven't been hydrated!!!
    val test = part5.players.map { p: SimPlayer[Option] =>
      val (pCard1, newDeck6) = getOrDeal(p.card1, part5.deck)
      val (pCard2, newDeck7) = getOrDeal(p.card2, newDeck6)
      val newP               = SimPlayer[Id](p.position, pCard1[Id], pCard2[Id])
    }

  }

  override def riverFrom: SimSetup[Option] => River = ???

  override def equity: List[River] => SimResult = ???
}
