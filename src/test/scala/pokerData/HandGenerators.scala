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

  val genFourOfAKind: Gen[FourOfAKind] =
    for {
      rank <- genRank
      hand          = Deck.all.groupBy(_.rank)(rank)
      quadsRankTest = Set(1, 2, 3, 4, 5, 6, 7, 9)
      deck          = Deck.all.filterNot(hand.contains(_))
      (_, cards)    = buildHand(deck, hand, quadsRankTest)
      rest          = cards.filterNot(hand.contains(_))
    } yield FourOfAKind(rank, rest)
//  yield Hand.rank(card1 :: card2 :: card3 :: quads)

  val genFullHouseCards: Gen[FullHouse] =
    for {
      (rank1, rank2) <- pick(2, Rank.all).map(x => (x.head, x.last))
      grouped = Deck.all.groupBy(_.rank)
      set  <- pick(3, grouped(rank1))
      pair <- pick(2, grouped(rank2))
      hand: List[Card]  = (set ++ pair).toList
      fullHouseRankTest = Set(1, 2, 3, 4, 5, 6, 8, 9)
      deck              = Deck.all.filterNot(hand.contains(_))
      (_, cards)        = buildHand(deck, hand, fullHouseRankTest)
      rest              = cards.filterNot(hand.contains(_))
    } yield FullHouse(set.head.rank, pair.head.rank)

  val genFullHouse: Gen[FullHouse] =
    for {
      (rank1, rank2) <- pick(2, Rank.all)
        .map(x => List(x.head, x.last).sortBy(_.value))
        .map(x => (x.head, x.last))
    } yield FullHouse(rank1, rank2)

  val genFlush: Gen[Flush] = for {
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
    rest             = cards.filterNot(hand.contains(_))

  } yield Flush(flush.head.rank, rest) // TODO Flushes don't have kickers...

  val genNonFlush: Gen[Hand] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    fourFlush <- pick(4, suited)
    deck             = Deck.all.filterNot(fourFlush.contains(_))
    hand: List[Card] = fourFlush.toList.sorted.reverse
    nonFlushRankTest = Set(6)
    (_, cards)       = buildHand(deck, hand, nonFlushRankTest)
    rest             = cards.filterNot(hand.contains(_))

  } yield Hand.rank(cards)

  val genWheelStraight: Gen[Straight] = {
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
      deck             = Deck.all.filterNot(hand.contains(_))
      (_, cards)       = buildHand(deck, hand, straightRankTest)
      rest             = cards.filterNot(hand.contains(_)) // TODO this is for the alt Gen

    } yield Straight(Five)
  }

  def genStraight_(high: Int): Gen[Straight] = {
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
      rest             = cards.filterNot(hand.contains(_))
    } yield Straight(ranks.head)
  }

  val genNonWheelStraight: Gen[Straight] =
    genStraight_(8)

  val genStraight: Gen[Straight] = {
    for {
      hand1     <- genWheelStraight
      hand2     <- genNonWheelStraight
      finalHand <- frequency((1, hand1), (10, hand2))
    } yield finalHand
  }

  val genNonNutStraight: Gen[Straight] = for {
    hand1     <- genWheelStraight
    hand2     <- genStraight_(7)
    finalHand <- frequency((1, hand1), (10, hand2))
  } yield finalHand

  val genStraightFlush: Gen[StraightFlush] = for {
    newSuit  <- genSuit
    straight <- genStraight
  } yield StraightFlush(straight.rank)

  val genThreeOfAKind: Gen[ThreeOfAKind] = for {
    rank <- genRank
    grouped: Map[Rank, List[Card]] = Deck.all.groupBy(_.rank)
    set <- pick(3, grouped(rank))
    deck                 = Deck.all.filterNot(_.rank == rank)
    hand: List[Card]     = set.toList
    threeOfaKindRankTest = Set(1, 2, 3, 5, 6, 7, 8, 9)
    (_, cards)           = buildHand(deck, hand, threeOfaKindRankTest)
    rest                 = cards.filterNot(hand.contains(_))
  } yield ThreeOfAKind(rank, rest)

  val genTwoPair: Gen[TwoPair] = for {
    rank1 <- genRank
    rank2 <- oneOf(Rank.all.filterNot(_ == rank1))
    grouped = Deck.all.groupBy(_.rank)
    pair1 <- pick(2, grouped(rank1))
    pair2 <- pick(2, grouped(rank2))
    hand: List[Card] = (pair1 ++ pair2).toList.sorted.reverse
    twoPairRankTest  = Set(1, 2, 4, 5, 6, 7, 8, 9)
    deck             = Deck.all.filterNot(hand.contains(_))
    (_, cards)       = buildHand(deck, hand, twoPairRankTest)
    rest             = cards.filterNot(hand.contains(_))

  } yield TwoPair(hand.head.rank, hand.last.rank, rest)

  val genPair: Gen[Pair] = for {
    rank <- genRank
    grouped: Map[Rank, List[Card]] = Deck.all.groupBy(_.rank)
    pair <- pick(2, grouped(rank))
    deck             = Deck.all.filterNot(_.rank == rank)
    hand: List[Card] = List(pair.head, pair(1))
    pairRankTest     = Set(1, 3, 4, 5, 6, 7, 8, 9)
    (_, cards)       = buildHand(deck, hand, pairRankTest)
    rest             = cards.filterNot(hand.contains(_))
  } yield Pair(rank, rest.sorted.reverse)

  val genHighCard: Gen[HighCard] = for {
    card <- genCard
    deck             = Deck.all.filterNot(_ === card)
    hand: List[Card] = List(card)
    highCardRankTest = Set(2, 3, 4, 5, 6, 7, 8, 9)
    (_, cards)       = buildHand(deck, hand, highCardRankTest)
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
      genFullHouseCards,
      genFourOfAKind,
      genStraightFlush
    )
}
