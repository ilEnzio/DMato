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

  val genNutStraightFlushCards: Gen[List[Card]] = {
    val broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    for {
      suit <- genSuit
      suitList = List.fill(5)(suit)
      hand = broadWayRanks
        .zip(suitList)
        .map(x => Card(x._1, x._2))
        .sorted
        .reverse
      straightFlushRankTest = Set(1, 2, 3, 4, 5, 6, 7, 8)
      deck                  = Deck.all.filterNot(hand.contains(_))
      (_, cards)            = buildHand(deck, hand, straightFlushRankTest)
    } yield cards
  }

  val genNutStraightFlush: Gen[StraightFlush] = for {
    cards <- genNutStraightFlushCards
  } yield StraightFlush(cards.drop(2).head.rank)

// ToDO - create
  val genNonNutStraightFlush: Gen[StraightFlush] = for {
    nonNut <- genStraightFlush.retryUntil(x => x.rank != Ace)
  } yield StraightFlush(nonNut.rank)

  val genDeucesFullOfTresCards: Gen[List[Card]] = {
    val rank1   = Two
    val rank2   = Three
    val grouped = Deck.all.groupBy(_.rank)
    for {
      set  <- pick(3, grouped(rank1))
      pair <- pick(2, grouped(rank2))
      hand              = (set ++ pair).toList
      fullHouseRankTest = Set(1, 2, 3, 4, 5, 6, 8, 9)
      deck = Deck.all.filterNot(x =>
        hand.contains(x) && x.rank === rank1 && x.rank === rank2
      )
      (_, cards) = buildHand(deck, hand, fullHouseRankTest)
    } yield cards
  }

  val genDeucesFullOfTres: Gen[FullHouse] =
    for {
      cards <- genDeucesFullOfTresCards
    } yield FullHouse(cards.drop(2).head.rank, cards.drop(2).last.rank)

  val genNutFlushCards: Gen[List[Card]] = for {
    suit <- genSuit
    wheelRanks    = List(Ace, Five, Four, Three, Two)
    broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    ranks <- pick(5, Rank.all).suchThat { x =>
      (x.contains(Ace) &&
      (x.sorted.reverse != wheelRanks) && (x.sorted.reverse != broadWayRanks))
    }
    hand          = ranks.map(Card(_, suit)).toList
    flushRankTest = Set(1, 2, 3, 4, 5, 7, 8, 9)
    deck          = Deck.all.filterNot(hand.contains(_))
    (_, cards)    = buildHand(deck, hand, flushRankTest)
  } yield cards

  val genNutFlush: Gen[Flush] = for {
    cards <- genNutFlushCards
  } yield Flush(
    cards.drop(2).head.rank,
    cards.take(2)
  ) // TODO Flushes don't have kickers

  val genNonNutFlushCards: Gen[List[Card]] = for {
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
//    rest             = cards.filterNot(hand.contains(_))

  } yield cards // TODO Flushes don't have kickers

  val genNonNutFlush: Gen[Flush] = for {
//    suit <- genSuit
//    suited = Deck.all.filter(_.suit == suit)
//    (ace, nonAces) = (
//      suited.filter(_.rank === Ace),
//      suited.filterNot(_.rank === Ace)
//    )
//    nonNutFlush <- pick(5, nonAces).retryUntil(x =>
//      Hand.rank(x.toList) match {
//        case _: StraightFlush => false
//        case _                => true
//      }
//    )
//    deck             = Deck.all.filterNot((ace ++ nonNutFlush).contains(_))
//    hand: List[Card] = nonNutFlush.toList.sorted.reverse
//    flushRankTest    = Set(1, 2, 3, 4, 5, 7, 8, 9)
//    (_, cards)       = buildHand(deck, hand, flushRankTest)
//    rest             = cards.filterNot(hand.contains(_))
    hand <- genNonNutFlushCards
  } yield Flush(
    hand.drop(2).head.rank,
    hand.drop(5).tail
  ) // TODO Flushes don't have kickers

  val genNutStraightCards: Gen[List[Card]] = {
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
    } yield cards
  }

  val genNutStraight: Gen[Straight] = for {
    hand <- genNutStraightCards
  } yield Straight(hand.drop(2).head.rank)

  val genAceHighCards: Gen[List[Card]] = {
    for {
      hand <- genHighCardCards
      suit <- genSuit
    } yield Card(Ace, suit) :: hand.sorted.reverse.tail
  }

  val genAceHigh = {
    for {
      hand <- genAceHighCards
    } yield HighCard(hand.head.rank, hand.tail)
  }

}
