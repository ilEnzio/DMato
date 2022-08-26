package test.pokerData

import org.scalacheck.Gen.{choose, oneOf, pick}
import org.scalacheck.{Arbitrary, Gen}
import poker._
import poker.Rank._
import poker.OrderInstances._

import scala.util.Random

object DataGenerators {

  val genSuit = Gen.oneOf(Suit.all)
  val genRank = Gen.oneOf(Rank.all)
  val genCard = for {
    rank <- genRank
    suit <- genSuit
  } yield Card(rank, suit)

  val startingDeck = Deck.makeStartingDeck //

  implicit val arbCard = Arbitrary(genCard)
  implicit val arbDeck = Arbitrary(startingDeck)

  val genHand = for {
    cards <- pick(7, Deck.all)
  } yield Hand(cards.toList)

  implicit val arbHand: Arbitrary[Hand] = Arbitrary(genHand)
  implicit val arbRank                  = Arbitrary(genRank)

  // Optimal Output Generators

// these generators are severely lacking.

  val genHighCard: Gen[Hand] = {
    val suits  = Random.shuffle(Suit.all)
    val offset = choose(0, 3).sample.get
    val hand1 = Hand(
      List(
        Card(rankMap(11 + offset), suits(0)),
        Card(rankMap(10 + offset), suits(0)),
        Card(rankMap(8 + offset), suits(1)),
        Card(rankMap(7 + offset), suits(1)),
        Card(rankMap(5 + offset), suits(2)),
        Card(rankMap(4 + offset), suits(2)),
        Card(rankMap(2 + offset), suits(3))
      )
    )
    val hand2 = Hand(
      List(
        Card(rankMap(11 + offset), suits(0)),
        Card(rankMap(9 + offset), suits(0)),
        Card(rankMap(8 + offset), suits(1)),
        Card(rankMap(6 + offset), suits(1)),
        Card(rankMap(5 + offset), suits(2)),
        Card(rankMap(3 + offset), suits(2)),
        Card(rankMap(2 + offset), suits(3))
      )
    )
    val hand3 = Hand(
      List(
        Card(rankMap(10 + offset), suits(0)),
        Card(rankMap(9 + offset), suits(0)),
        Card(rankMap(8 + offset), suits(1)),
        Card(rankMap(7 + offset), suits(1)),
        Card(rankMap(5 + offset), suits(2)),
        Card(rankMap(4 + offset), suits(2)),
        Card(rankMap(2 + offset), suits(3))
      )
    )
    val variations = List(hand1, hand2, hand3)
    oneOf(variations)
  }

  val genAceHigh: Gen[Hand] = {
    for {
      hand <- genHighCard
      suit <- genSuit
    } yield Hand(Card(Ace, suit) :: hand.cards.sorted.reverse.tail)
  }

  val genStraightFlush: Gen[Hand] = for {
    newSuit  <- genSuit
    straight <- genStraight
  } yield Hand(straight.cards.distinctBy(_.rank).map(x => x.copy(suit = newSuit)))

  val genNutStraightFlush: Gen[Hand] = {
    val broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    for {
      suit <- genSuit
      suitList = List.fill(5)(suit)
    } yield Hand(broadWayRanks.zip(suitList).map(x => Card(x._1, x._2)))
  }

  val genNutStraightFlush2: Gen[Hand] = {

    for {
      broadway <- genNutStraight
      newSuit  <- genSuit
    } yield Hand(broadway.cards.map(c => c.copy(suit = newSuit)))
  }

  val genNonNutStraightFlush: Gen[Hand] = for {
    newSuit  <- genSuit
    straight <- genStraight.suchThat(x => !x.cards.map(_.rank).contains(Ace))
    cards = straight.cards.distinct
  } yield Hand(cards.map(_.copy(suit = newSuit)))

  val genFourOfAKind: Gen[Hand] =
    for {
      rank <- genRank
      quads = Deck.all.groupBy(_.rank)(rank)
      card1 <- genCard.suchThat(c => c.rank != rank)
      card2 <- genCard.suchThat(c => c != card1 && c.rank != rank)
      card3 <- genCard.suchThat(c => !List(card1, card2).contains(c) && c.rank != rank)
    } yield Hand(card1 :: card2 :: card3 :: quads)

  val genFullHouse: Gen[Hand] =
    for {
      rank1 <- genRank
      rank2 <- genRank.suchThat(r => r != rank1)
      grouped = Deck.all.groupBy(_.rank)
      set  <- pick(3, grouped(rank1))
      pair <- pick(2, grouped(rank2))
      card1 <- genCard.suchThat { c =>
        !set.contains(c) &&
        !pair.contains(c) &&
        c.rank != rank1 &&
        c.rank != rank2
      }
      card2 <- genCard.suchThat(c =>
        !set.contains(c) &&
          !pair.contains(c) &&
          c.rank != rank1 &&
          c.rank != rank2 &&
          c != card1
      )
    } yield Hand(card1 :: card2 :: pair.toList ++ set.toList)

  val genDeucesFullOfTres: Gen[Hand] = {
    val rank1   = Two
    val rank2   = Three
    val grouped = Deck.all.groupBy(_.rank)

    for {
      set  <- pick(3, grouped(rank1))
      pair <- pick(2, grouped(rank2))
    } yield Hand(pair.toList ++ set.toList)
  }

  val genFlush: Gen[Hand] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    flush <- pick(5, suited)
    card1 <- genCard.suchThat { c =>
      !suited.contains(c)
    }
    card2 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1
    }
  } yield Hand(card1 :: card2 :: flush.toList)

  val genNutFlush: Gen[Hand] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    flush <- pick(5, suited).retryUntil(x =>
      x.contains(Card(Ace, suit)) &&
        HandRank(Hand(x.toList)) != StraightFlush
    )
    card1 <- genCard.suchThat { c =>
      !suited.contains(c)
    }
    card2 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1
    }
  } yield Hand(card1 :: card2 :: flush.toList)

  val genNonNutFlush: Gen[Hand] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    flush <- pick(5, suited).retryUntil(x =>
      !x.contains(Card(Ace, suit)) &&
        HandRank(Hand(x.toList)) != StraightFlush
    )
  } yield Hand(flush.toList)

  val genNonFlush: Gen[Hand] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    fourFlush <- pick(4, suited)
    card1 <- genCard.suchThat { c =>
      !suited.contains(c)
    }
    card2 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1
    }
    card3 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1 &&
      c != card2
    }
  } yield Hand(card1 :: card2 :: card3 :: fourFlush.toList)

  /// group card by rank; pick a rank between A-5
  // take a card from that top rank and 4 ranks beneath it.
  // 5 is a special case
  // 0-2, 1-3,2-4,3-5,4-6,5-7,6-8,7-9,8-10,9-11,10-12,11-12,12-14
  val genWheelStraight: Gen[Hand] = {
    val wheelRanks = List(Ace, Five, Four, Three, Two)
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.suchThat(s => List(suit1, suit2, suit3, suit4).count(_ == s) < 4)
      suits = List(suit1, suit2, suit3, suit4, suit5)
    } yield Hand(wheelRanks.zip(suits).map(x => Card(x._1, x._2)))
  }

  def genNutStraight: Gen[Hand] = {
    val broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.suchThat(s => List(suit1, suit2, suit3, suit4).count(_ == s) < 4)
      suits = List(suit1, suit2, suit3, suit4, suit5)
    } yield Hand(broadWayRanks.zip(suits).map(x => Card(x._1, x._2)))
  }

  val genStraight: Gen[Hand] = {
    for {
      hand1     <- genWheelStraight
      hand2     <- genNonWheelStraight
      finalHand <- oneOf(List(hand1, hand2))
    } yield finalHand
  }

//  val genNonNutStraight: Gen[Hand] = // broken
//    genStraight_(7)

  val genNonWheelStraight: Gen[Hand] =
    genStraight_(8)

  def genStraight_(high: Int) = {
    val idx                               = choose(0, high).sample.get
    val grouped: List[(Rank, List[Card])] = Deck.all.groupBy(_.rank).toList.sortBy(_._1)
    val hslice: List[(Rank, List[Card])]  = grouped.slice(idx, idx + 5)
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.suchThat(s => List(suit1, suit2, suit3, suit4).count(_ == s) < 4)
      suit6 <- genSuit.suchThat(s => List(suit1, suit2, suit3, suit4, suit5).count(_ == s) < 4)
      suit7 <- genSuit.suchThat(s => List(suit1, suit2, suit3, suit4, suit5, suit6).count(_ == s) < 4)
      c1    <- Gen.oneOf(hslice(0)._2)
      c2    <- Gen.oneOf(hslice(1)._2)
      c3    <- Gen.oneOf(hslice(2)._2)
      c4    <- Gen.oneOf(hslice(3)._2)
      c5    <- Gen.oneOf(hslice(4)._2)
      n1    <- genCard.suchThat(c => !List(c1, c2, c3, c4, c5).contains(c))
      n2    <- genCard.suchThat(c => !List(c1, c2, c3, c4, c5, n1).contains(c))
    } yield Hand(
      List(
        c1.copy(suit = suit1),
        c2.copy(suit = suit2),
        c3.copy(suit = suit3),
        c4.copy(suit = suit4),
        c5.copy(suit = suit5),
        n1.copy(suit = suit6),
        n2.copy(suit = suit7)
      )
    )
  }

  val genThreeOfAKind: Gen[Hand] = for {
    ranks <- pick(5, Rank.all).retryUntil { r =>
      val x = r.toList.map(_.value).sorted
      x(0) + 4 != x(4) && x(0) + 12 != x(4)
    }
    suits <- pick(3, Suit.all)
    set = List(Card(ranks(0), suits(0)), Card(ranks(0), suits(1)), Card(ranks(0), suits(2)))
    card1 <- genCard.retryUntil(c =>
      c.rank == ranks(1) &&
        set.map(_.suit).contains(c.suit)
    ) // 1 of suit already
    card2 <- genCard.retryUntil(c =>
      c.rank == ranks(2) &&
        c.suit == (card1 :: set).groupBy(_.suit).filter(_._2.size == 1).toList.head._1
    ) // 1 of suit already
    card3 <- genCard.retryUntil(c =>
      c.rank == ranks(3) &&
        c.suit == (card1 :: card2 :: set).groupBy(_.suit).filter(_._2.size == 2).toList.head._1
    ) // 2 of suit already
    card4 <- genCard.retryUntil(c =>
      c.rank == ranks(4) &&
        !(card1 :: card2 :: card3 :: set).map(_.suit).contains(c.suit)
    ) // unique suit
  } yield Hand(card1 :: card2 :: card3 :: card4 :: set)

  val genTwoPair: Gen[Hand] = for {
    rank1 <- genRank
    rank2 <- genRank.retryUntil(_ != rank1)
    grouped = Deck.all.groupBy(_.rank)
    pair1 <- pick(2, grouped(rank1))
    pair2 <- pick(2, grouped(rank2))
    card1 <- genCard.retryUntil(c => !List(rank1, rank2).contains(c.rank))
    card2 <- genCard.retryUntil(c => c != card1 && !List(rank1, rank2).contains(c.rank))
    card3 <- genCard.retryUntil(c =>
      !List(card1, card2).contains(c) && !List(rank1, rank2, card2.rank).contains(c.rank) &&
        !List(card1, card2).map(_.suit).contains(c.suit) &&
        HandRank(Hand(card1 :: card2 :: c :: pair1.toList ++ pair2.toList)) != Straight
    )
  } yield Hand(card1 :: card2 :: card3 :: pair1.toList ++ pair2.toList)

  val genPair: Gen[Hand] = for {
    rank <- genRank
    grouped = Deck.all.groupBy(_.rank)
    pair <- pick(2, grouped(rank))
    rankingList = HandRank.all.filterNot(_ == Pair)
    hand <- genHand.suchThat(h => !rankingList.contains(HandRank(Hand(pair.toList ++ h.cards.take(5)))))
  } yield Hand(pair.toList ++ hand.cards.take(5))

}
