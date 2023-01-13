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

  val genFourOfAKindCards: Gen[List[Card]] =
    for {
      rank <- genRank
      hand = Deck.all.groupBy(_.rank)(rank)
      deck = Deck.all.filterNot(hand.contains(_))
      remaining <- pick(3, deck)
    } yield remaining.toList ++ hand

  val genFourOfAKind: Gen[FourOfAKind] =
    for {
      cards <- genFourOfAKindCards
    } yield FourOfAKind(cards.drop(3).head.rank, cards.take(3).sorted.reverse)

  // TODO get rid of buildHand; make the code less generic. make each generator specific
  // to the hand constraints

  def genFullHouseCards_(setRank: Rank, pairRank: Rank): Gen[List[Card]] = {
    val grouped = Deck.all.groupBy(_.rank)
    for {
      set  <- pick(3, grouped(setRank))
      pair <- pick(2, grouped(pairRank))
      deck = Deck.all.filterNot(x => x.rank === setRank || x.rank === pairRank)
      remaining <- pick(2, deck)
    } yield (remaining ++ set ++ pair).toList
  }
  val genFullHouseCards: Gen[List[Card]] = {
    for {
      (setRank, pairRank) <-
        pick(2, Rank.all).map(x => (x.head, x.last))
      cards <- genFullHouseCards_(setRank, pairRank)
    } yield cards
  }

  val genFullHouse: Gen[FullHouse] =
    for {
      cards <- genFullHouseCards
    } yield FullHouse(cards.drop(2).head.rank, cards.last.rank)

  val genFlushCards: Gen[List[Card]] = for {
    suit <- genSuit
    suited = Deck.all.filter(x => x.suit == suit)
    flush <- pick(5, suited).suchThat(x =>
      Hand.rank(x.toList) match {
        case _: StraightFlush => false
        case _                => true
      }
    )
    deck             = Deck.all.filterNot(flush.contains(_))
    hand: List[Card] = flush.toList.sorted.reverse
    // TODO having trouble eliminating this manual "retry until" operation
    flushRankTest = Set(1, 2, 3, 4, 5, 7, 8, 9)
    (_, cards)    = buildHand(deck, hand, flushRankTest)
  } yield cards

  val genFlush: Gen[Flush] = for {
    cards <- genFlushCards
  } yield Flush(
    cards.drop(2).head.rank,
    cards.take(2)
  ) // TODO Flushes don't have kickers...

  val genFourFlushCards: Gen[List[Card]] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    fourFlush <- pick(4, suited)
    deck             = Deck.all.filterNot(_.suit == suit)
    hand: List[Card] = fourFlush.toList.sorted.reverse
    nonFlushRankTest = Set(6, 9)
    (_, cards)       = buildHand(deck, hand, nonFlushRankTest)
  } yield cards

  val genFourFlush: Gen[Hand] = genFourFlushCards.map(Hand.rank)

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

  def genStraightCards_(highestRank: Rank): Gen[List[Card]] = {

    val newRanks = Rank.all.filter(_ <= highestRank)

    for {
      idx <- choose(0, newRanks.length - 4)
      ranks: List[Rank] = Rank.all.slice(idx, idx + 5).sorted.reverse
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

  def genStraight_(highestRank: Rank): Gen[Straight] = for {

    cards <- genStraightCards_(highestRank)
  } yield Straight(cards.drop(2).head.rank)

  val genNonWheelStraightCards: Gen[List[Card]] =
    genStraightCards_(Ace)

  val genNonWheelStraight: Gen[Straight] =
    genStraight_(Ace)

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
    hand2     <- genStraightCards_(King)
    finalHand <- frequency((1, hand1), (10, hand2))
  } yield finalHand

  val genNonNutStraight: Gen[Straight] = for {
    hand1     <- genWheelStraight
    hand2     <- genStraight_(King)
    finalHand <- frequency((1, hand1), (10, hand2))
  } yield finalHand

  val genStraightFlushCards: Gen[List[Card]] = for {
    newSuit  <- genSuit
    straight <- genStraightCards
    cards = straight.map(x => Card(x.rank, newSuit)).distinct
    deck  = Deck.all.filterNot(cards.contains(_))
    remaining <- pick(2, deck)
  } yield remaining.toList ++ cards

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

  val genHandCards: Gen[List[Card]] =
    oneOf(
      genHighCardCards,
      genPairCards,
      genTwoPairCards,
      genThreeOfAKindCards,
      genStraightCards,
      genFlushCards,
      genFullHouseCards,
      genFourOfAKindCards,
      genStraightFlushCards
    )

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
