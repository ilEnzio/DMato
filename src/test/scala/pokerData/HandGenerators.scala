package pokerData

import cats.implicits._
import org.scalacheck.Gen._
import org.scalacheck.Gen
import poker._
import pokerData.DeckGenerators._
import poker.Hand._
import poker.OrderInstances._

import scala.annotation.tailrec

object HandGenerators {
//So the strange thing here is that it's not generating the cards involved.

  val genFourOfAKindCards: Gen[List[Card]] =
    for {
      rank <- genRank
      hand          = Deck.all.groupBy(_.rank)(rank)
      quadsRankTest = Set(1, 2, 3, 4, 5, 6, 7, 9)
      deck          = Deck.all.filterNot(hand.contains(_))
      (_, cards)    = buildHand(deck, hand, quadsRankTest)
    } yield cards

  val genFourOfAKind: Gen[FourOfAKind] =
    for {
      cards <- genFourOfAKindCards
    } yield FourOfAKind(cards.drop(3).head.rank, cards.take(3).sorted.reverse)

  val genFullHouseCards: Gen[List[Card]] =
    for {
      (rank1, rank2) <- pick(2, Rank.all).map(x => (x.head, x.last))
      grouped = Deck.all.groupBy(_.rank)
      set  <- pick(3, grouped(rank1))
      pair <- pick(2, grouped(rank2))
      hand: List[Card]  = (set ++ pair).toList
      fullHouseRankTest = Set(1, 2, 3, 4, 5, 6, 8, 9)
      deck              = Deck.all.filterNot(hand.contains(_))
      (_, cards)        = buildHand(deck, hand, fullHouseRankTest)
    } yield cards

  val genFullHouse: Gen[FullHouse] =
    for {
      cards <- genFullHouseCards
    } yield FullHouse(cards.drop(2).head.rank, cards.drop(2).last.rank)

  val genFlushCards: Gen[List[Card]] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    flush <- pick(5, suited).suchThat(x =>
      Hand.rank(x.toList) match {
        case _: StraightFlush => false
        case _                => true
      }
    )
    deck             = Deck.all.filterNot(flush.contains(_))
    hand: List[Card] = flush.toList.sorted.reverse
    flushRankTest    = Set(1, 2, 3, 4, 5, 7, 8, 9)
    (_, cards)       = buildHand(deck, hand, flushRankTest)
  } yield cards

  val genFlush: Gen[Flush] = for {
    cards <- genFlushCards
  } yield Flush(
    cards.drop(2).head.rank,
    cards.take(2)
  ) // TODO Flushes don't have kickers...

  val genNonFlushCards: Gen[List[Card]] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    fourFlush <- pick(4, suited)
    deck             = Deck.all.filterNot(fourFlush.contains(_))
    hand: List[Card] = fourFlush.toList.sorted.reverse
    nonFlushRankTest = Set(6)
    (_, cards)       = buildHand(deck, hand, nonFlushRankTest)
  } yield cards

  val genNonFlush: Gen[Hand] = for {
    cards <- genNonFlushCards
  } yield Hand.rank(cards)

  val genWheelStraightCards: Gen[List[Card]] = {
    val wheelRanks = List(Ace, Five, Four, Three, Two)
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.suchThat(s =>
        List(suit1, suit2, suit3, suit4).count(_ == s) < 4
      )
      suits            = List(suit1, suit2, suit3, suit4, suit5)
      hand: List[Card] = wheelRanks.zip(suits).map(x => Card(x._1, x._2))
      straightRankTest = Set(1, 2, 3, 4, 6, 7, 8, 9)
      deck             = Deck.all.filterNot(c => hand.contains(c) && (c.rank === Six))
      (_, cards)       = buildHand(deck, hand, straightRankTest)

    } yield cards
  }
  val genWheelStraight: Gen[Straight] = for {
    cards <- genWheelStraightCards
  } yield Straight(cards.drop(2)(1).rank)

  def genStraightCards_(high: Int): Gen[List[Card]] = {
    val idx               = choose(0, high).sample.get
    val ranks: List[Rank] = Rank.all.slice(idx, idx + 5).sorted.reverse
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.retryUntil(s =>
        List(suit1, suit2, suit3, suit4).count(_ == s) < 4
      )
      suits            = List(suit1, suit2, suit3, suit4, suit5)
      hand: List[Card] = ranks.zip(suits).map(x => Card(x._1, x._2))
      straightRankTest = Set(1, 2, 3, 4, 6, 7, 8, 9)
      deck             = Deck.all.filterNot(hand.contains(_))
      (_, cards)       = buildHand(deck, hand, straightRankTest)
    } yield cards
  }

  def genStraight_(high: Int): Gen[Straight] = for {

    cards <- genStraightCards_(high)
  } yield Straight(cards.drop(2).head.rank)

  val genNonWheelStraightCards: Gen[List[Card]] =
    genStraightCards_(8)

  val genNonWheelStraight: Gen[Straight] =
    genStraight_(8)

  val genStraightCards: Gen[List[Card]] = {
    for {
      hand1      <- genWheelStraightCards
      hand2      <- genNonWheelStraightCards
      finalCards <- frequency((1, hand1), (10, hand2))
    } yield finalCards
  }
  val genStraight: Gen[Straight] = {
    for {
      hand1     <- genWheelStraight
      hand2     <- genNonWheelStraight
      finalHand <- frequency((1, hand1), (10, hand2))
    } yield finalHand
  }

  val genNonNutStraightCards: Gen[List[Card]] = for {
    hand1     <- genWheelStraightCards
    hand2     <- genStraightCards_(7)
    finalHand <- frequency((1, hand1), (10, hand2))
  } yield finalHand

  val genNonNutStraight: Gen[Straight] = for {
    hand1     <- genWheelStraight
    hand2     <- genStraight_(7)
    finalHand <- frequency((1, hand1), (10, hand2))
  } yield finalHand

  val genStraightFlushCards: Gen[List[Card]] = for {
    newSuit  <- genSuit
    straight <- genStraightCards
    hand             = straight.map(x => Card(x.rank, newSuit)).distinct
    straightRankTest = Set(1, 2, 3, 4, 5, 6, 7, 8)
    deck             = Deck.all.filterNot(hand.contains(_))
    (_, cards)       = buildHand(deck, hand, straightRankTest)
  } yield cards

  val genStraightFlush: Gen[StraightFlush] = for {
    straightFlush <- genStraightFlushCards
  } yield StraightFlush(straightFlush.drop(2).sorted.reverse.head.rank)

  val genThreeOfAKindCards: Gen[List[Card]] = for {
    rank <- genRank
    grouped: Map[Rank, List[Card]] = Deck.all.groupBy(_.rank)
    set <- pick(3, grouped(rank))
    deck                 = Deck.all.filterNot(_.rank == rank)
    hand: List[Card]     = set.toList
    threeOfaKindRankTest = Set(1, 2, 3, 5, 6, 7, 8, 9)
    (_, cards)           = buildHand(deck, hand, threeOfaKindRankTest)
  } yield cards

  val genThreeOfAKind: Gen[ThreeOfAKind] = for {
    cards <- genThreeOfAKindCards
  } yield ThreeOfAKind(cards.head.rank, cards.drop(3).sorted.reverse)

  val genTwoPairCards: Gen[List[Card]] = for {
    rank1 <- genRank
    rank2 <- oneOf(Rank.all.filterNot(_ == rank1))
    grouped = Deck.all.groupBy(_.rank)
    pair1 <- pick(2, grouped(rank1))
    pair2 <- pick(2, grouped(rank2))
    hand: List[Card] = (pair1 ++ pair2).toList.sorted.reverse
    twoPairRankTest  = Set(1, 2, 4, 5, 6, 7, 8, 9)
    deck             = Deck.all.filterNot(hand.contains(_))
    (_, cards)       = buildHand(deck, hand, twoPairRankTest)
  } yield cards

  val genTwoPair: Gen[TwoPair] = for {
    cards <- genTwoPairCards
  } yield TwoPair(cards.head.rank, cards(2).rank, cards.drop(4).sorted.reverse)

  val genPairCards: Gen[List[Card]] = for {
    rank <- genRank
    grouped: Map[Rank, List[Card]] = Deck.all.groupBy(_.rank)
    pair <- pick(2, grouped(rank))
    deck             = Deck.all.filterNot(_.rank == rank)
    hand: List[Card] = List(pair.head, pair(1))
    pairRankTest     = Set(1, 3, 4, 5, 6, 7, 8, 9)
    (_, cards)       = buildHand(deck, hand, pairRankTest)
  } yield cards

  val genPair: Gen[Pair] = for {
    cards <- genPairCards
  } yield Pair(cards.head.rank, cards.drop(2).sorted.reverse)

  val genHighCardCards: Gen[List[Card]] = for {
    card <- genCard
    deck             = Deck.all.filterNot(_ === card)
    hand: List[Card] = List(card)
    highCardRankTest = Set(2, 3, 4, 5, 6, 7, 8, 9)
    (_, cards)       = buildHand(deck, hand, highCardRankTest)
  } yield cards

  val genHighCard: Gen[HighCard] = for {
    cards <- genHighCardCards
  } yield HighCard(cards.head.rank, cards.tail)

  // TODO Find HOF to replace hand rolled recursion?
  @tailrec
  def buildHand(
    deck: List[Card],
    cards: List[Card],
    rankTest: Int => Boolean
  ): (List[Card], List[Card]) =
    if (cards.length === 7) (deck, cards) // TODO magic number
    else {
      val newDeck    = deck.filterNot(cards.contains(_))
      val card: Card = oneOf(newDeck).sample.get // TODO get !!!
      if (rankTest(Hand.rank(card :: cards).score))
        buildHand(newDeck, cards, rankTest)
      else
        buildHand(newDeck.filterNot(_ === card), card :: cards, rankTest)
    }

  val genHand: Gen[Hand] =
    oneOf(
      genHighCard,
      genPair,
      genTwoPair,
      genThreeOfAKind,
      genStraight,
      genFlush,
      genFullHouse,
      genFourOfAKind,
      genStraightFlush
    )
}
