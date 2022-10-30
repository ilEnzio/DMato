package pokerData

import org.scalacheck.Gen.pick
import org.scalacheck.Gen
import poker.Deck.{StartingDeck}
import poker._
import pokerData.DeckGenerators._
import pokerData.HandGenerators._
import poker.Hand.{Flush, FullHouse, HighCard, Straight, StraightFlush}
import poker.OrderInstances.cardOrdering

object SpecialHandsGenerators {

  val genNutStraightFlush: Gen[StraightFlush] = {
    val broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    for {
      suit <- genSuit
      suitList = List.fill(5)(suit)
    } yield Hand
      .rank(broadWayRanks.zip(suitList).map(x => Card(x._1, x._2)))
      .asInstanceOf[StraightFlush]
  }

  val genNonNutStraightFlush: Gen[StraightFlush] = for {
    nonNut <- genStraightFlush.retryUntil(x => x.rank != Ace)
  } yield StraightFlush(nonNut.rank)

  val genDeucesFullOfTres: Gen[FullHouse] = {
    val rank1   = Two
    val rank2   = Three
    val grouped = Deck.all.groupBy(_.rank)

    for {
      set  <- pick(3, grouped(rank1))
      pair <- pick(2, grouped(rank2))
    } yield Hand.rank(pair.toList ++ set.toList).asInstanceOf[FullHouse]
  }

  val genNutFlush: Gen[Flush] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    flush <- pick(5, suited).retryUntil(x =>
      x.contains(Card(Ace, suit)) &&
        (Hand.rank(x.toList) match {
          case _: StraightFlush => false
          case _                => true
        }) /// ToDo ????
    )
    card1 <- genCard.suchThat { c =>
      !suited.contains(c)
    }
    card2 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1
    }
  } yield Hand.rank(card1 :: card2 :: flush.toList).asInstanceOf[Flush]

  val genNonNutFlush: Gen[Flush] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    flush <- pick(5, suited).retryUntil(x =>
      !x.contains(Card(Ace, suit)) &&
        (Hand.rank(x.toList) match {
          case _: StraightFlush => false
          case _                => true
        }) // todo Again...???
    )
  } yield Hand.rank(flush.toList).asInstanceOf[Flush]

  val genNutStraight: Gen[Straight] = {
    val broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.suchThat(s =>
        List(suit1, suit2, suit3, suit4).count(_ == s) < 4
      )
      suits = List(suit1, suit2, suit3, suit4, suit5)
    } yield Hand
      .rank(broadWayRanks.zip(suits).map(x => Card(x._1, x._2)))
      .asInstanceOf[Straight]
  }

  val genAceHigh: Gen[HighCard] = {
    for {
      hand <- genHighCard
      suit <- genSuit
    } yield HighCard(Ace, Card(Ace, suit) :: hand.kickers.sorted.reverse.tail)
  }

}
