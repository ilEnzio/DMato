package poker.showdowns

import cats.implicits.catsSyntaxEitherId
import cats.kernel.Monoid
import org.scalactic.anyvals.NonEmptySet
import poker.{Player, Position, PreflopDeck, ShowDown, WinnerList}
import cats.kernel.Monoid

import java.time.LocalDateTime

sealed trait Event[A] {
  def timeStamp: LocalDateTime
}

case class PreFlopShownDown(
  winners: WinnerList,
  timeStamp: LocalDateTime = LocalDateTime.now()
) extends Event[WinnerList]

case class FlopShownDown(
  winners: WinnerList,
  timeStamp: LocalDateTime = LocalDateTime.now()
) extends Event[WinnerList]

case class TurnShowDown(
  winners: WinnerList,
  timeStamp: LocalDateTime = LocalDateTime.now()
) extends Event[WinnerList]

case class RiverShownDown(
  winners: WinnerList,
  timeStamp: LocalDateTime = LocalDateTime.now()
) extends Event[WinnerList]

object Common {
  type AggregateId = String
  type Error       = String
}

import Common._

trait Aggregate {
  def id: AggregateId
}

trait Snapshot[A <: Aggregate] {
  def updateState(e: Event[_], initial: A): A

//  def snapshot(es: List[Event[_]]): Either[String, A] =
//    es.reverse
//      .foldLeft(Monoid[A].empty) { (s, v) =>
//        updateState(v, s)
//      }
//      .asRight
}

//object ShowdownSnapshot extends Snapshot[WinnerList] {
//  def updateState(
//    e: Event[_],
//    initial: WinnerList
//  ): WinnerList = e match {
//    case PreFlopShownDown(winners, timeStamp) => ???
//    case FlopShownDown(winners, timeStamp)    => ???
//    case TurnShowDown(winners, timeStamp)     => ???
//    case RiverShownDown(winners, timeStamp)   => ???
//  }
//
//}
