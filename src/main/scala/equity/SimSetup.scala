package equity

import cats.Id
import poker.Card

final case class SimSetup[F[_]](
  players: List[SimPlayer],
  card1: F[Card],
  card2: F[Card],
  card3: F[Card],
  turn: F[Card]
) {}

sealed trait EquityService {
  def hydrateSim(simSetup: SimSetup[Option]): SimSetup[Id]
  def runSim[F[_]](simSetup: SimSetup[Id]): F[ShowdownResult]
  //  def calculateEquity(results: ShowdownResult): EquityCalculation

}
