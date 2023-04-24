//package equity
//
//import cats.syntax.all._
//import cats.effect.std.Random
//import cats.{Functor, Monoid}
//import org.scalactic.anyvals.NonEmptySet
////import poker.Deck.{equityStartingDeck}
//import poker.{Card, Position}
////import cats.data.NonEmptySet
//import cats.implicits.toFoldableOps
//import poker.OrderInstances.handOrder
//import poker.{Hand, Player, Street}
//
//sealed trait EquityService[ShowdownResult, EquityCalculation] {
//
////  def runSim[F[_]: Functor: Random](
////    setUp: SimSetup
////  ): F[ShowdownResult]                                            // Showdown result
//  def calculateEquity(results: ShowdownResult): EquityCalculation // ???
//
//  // TODO I think I want to build/process a stream of ShowdownResult
//  // into ??updating?? EquityCalculation?
//
//  // Stream[IO, ShowdownResult]
//  //Do I just take the simSetup and repeat it n times;
//  // then map that through an equity calculator?
//
//}
//
//object EquityService extends EquityService[ShowdownResult, EquityCalculation] {
//  override def runSim[F[_]: Functor: Random](
//    setUp: SimSetup[Complete]
//  ): F[ShowdownResult] = {
//    val dealtCards = setUp match {
//      case SimSetup(l, x, y, z, t) =>
//        l.flatMap(x => List(x.card1, x.card2)) ::: List(x, y, z, t, None)
//    }
//
//    for {
//      shuffledCards <- equityStartingDeck.shuffle
//
//      // remove all the non-None cards in the simSetup from the shuffledCards
//      liveCards = shuffledCards.filterNot(dealtCards.flatten.contains(_))
//
//      // option List of card
//      cards = liveCards.take(dealtCards.count(_ == None))
//      test = dealtCards.map { x =>
//        x match {
//          case Some(c) => c
//          case None    => ??? // deal a card from liveCards
//        }
//      }
//
//      // Build a River by dealing card.some into the spots with none,
//      // then mapping the cards to the players and then the board
//
//      // then handling that board with the ShowDown object
//
//    } yield liveCards
//    // I need: all the player hole cards provided, the remaining deck,
//    //  the number of hole cards needed
//  }
//  override def calculateEquity(results: ShowdownResult): EquityCalculation = ???
//}
//
//final case class SimPlayer(
//  position: Position,
//  card1: Option[Card],
//  card2: Option[Card]
//) {}
//
//final case class SimSetup[Status](
//  players: List[SimPlayer],
//  card1: Option[Card],
//  card2: Option[Card],
//  card3: Option[Card],
//  turn: Option[Card]
//) {
//  trait NeedFlopCards
//  trait Need2FlopCards
//  trait Need1FlopCard
//  trait NeedTurnCard
//  trait Complete
//
//  type SimSetupNeedFlopCards  = SimSetup[NeedFlopCards]
//  type SimSetupNeed2FlopCards = SimSetup[Need2FlopCards]
//  type SimSetupNeed1FlopCard  = SimSetup[Need1FlopCard]
//  type SimSetupNeedTurnCard   = SimSetup[NeedTurnCard]
//  type SimComplete            = SimSetup[Complete]
//
////  def runSim[F[_]: Functor: Random](
////    setUp: SimSetup[Status]
////  ): F[SimComplete] // Showdown result
////  def calculateEquity(results: ShowdownResult): EquityCalculation
//
//}
//
//object PlayerStanding {
//  def apply(board: Street): List[(Int, Player, Hand)] =
//    board.players
//      .zip(board.allHands)
//      .zipWithIndex
//      .map { case ((p, h), i) => (i, p, h) }
//
//  //TODO this must be wrong??
//  def winnerList(board: Street): Option[NonEmptySet[(Int, Player, Hand)]] = {
//    val winners: Set[(Int, Player, Hand)] = PlayerStanding(board)
//      .maximumByList[Hand](x => x._3)
//      .toSet
//
//    NonEmptySet.from(winners)
//  }
//}
//
//final case class ShowdownResult(results: Map[Position, Int])
//object ShowdownResult {
////  def initial(n: Int)(implicit m: Monoid[ShowdownResult]): ShowdownResult =
//  ////    (1 to n)
//  ////      .map(x => ShowdownResult(Map(x -> 0)))
//  ////      .foldLeft(m.empty)(m.combine)
//
//  implicit val showdownResultMonoid: Monoid[ShowdownResult] =
//    new Monoid[ShowdownResult] {
//      override def empty: ShowdownResult = ShowdownResult(Map.empty)
//
//      override def combine(
//        x: ShowdownResult,
//        y: ShowdownResult
//      ): ShowdownResult =
//        ShowdownResult(y.results.foldLeft(x.results) { (s, v) =>
//          if (!s.contains(v._1)) s + v
//          else s + (v._1 -> (v._2 + s(v._1)))
//        })
//    }
//}
//
//final case class EquityCalculation(equityCalculation: Map[Position, Double])
