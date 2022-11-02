package pokerData

import cats.implicits._
import org.scalacheck.Gen._
import org.scalacheck.Gen
import poker._
import pokerData.DeckGenerators._
import pokerData.HandGenerators._
import poker.Hand._
import poker.OrderInstances._

object SpecialHandsGenerators {

  val genNutStraightFlush: Gen[StraightFlush] = {
    val broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    for {
      suit <- genSuit
      suitList = List.fill(5)(suit)
      cards = broadWayRanks
        .zip(suitList)
        .map(x => Card(x._1, x._2))
        .sorted
        .reverse

    } yield StraightFlush(cards.head.rank)
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
    } yield FullHouse(set.head.rank, pair.head.rank)
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
    hand = flush.toList.sorted.reverse
    rest = List(card1, card2)
  } yield Flush(hand.head.rank, rest) // TODO Flushes don't have kickers

  val genNonNutFlush: Gen[Flush] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    (ace, nonAces) = (
      suited.filter(_.rank === Ace),
      suited.filterNot(_.rank === Ace)
    )
    nonNutFlush <- pick(5, nonAces).retryUntil(x =>
      Hand.rank(x.toList) match {
        case _: StraightFlush => false
        case _                => true
      }
    )
    deck             = Deck.all.filterNot((ace ++ nonNutFlush).contains(_))
    hand: List[Card] = nonNutFlush.toList.sorted.reverse
    flushRankTest    = Set(1, 2, 3, 4, 5, 7, 8, 9)
    (_, cards)       = buildHand(deck, hand, flushRankTest)
    rest             = cards.filterNot(hand.contains(_))

  } yield Flush(hand.head.rank, rest) // TODO Flushes don't have kickers

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
      hand: List[Card] = broadWayRanks
        .zip(suits)
        .map(x => Card(x._1, x._2))
        .sorted
        .reverse
      straightRankTest = Set(1, 2, 3, 4, 6, 7, 8, 9)
      deck             = Deck.all.filterNot(hand.contains(_))
      (_, cards)       = buildHand(deck, hand, straightRankTest)
      rest             = cards.filterNot(hand.contains(_))
    } yield Straight(hand.head.rank)
  }

  val genAceHigh: Gen[HighCard] = {
    for {
      hand <- genHighCard
      suit <- genSuit
    } yield HighCard(Ace, Card(Ace, suit) :: hand.kickers.sorted.reverse.tail)
  }

}
