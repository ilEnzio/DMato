package pokerData

import org.scalacheck.Gen.{choose, frequency, oneOf, pick}
import org.scalacheck.Gen
import poker._
import poker.{Deck, Hand, Rank}
import poker.Rank._
import pokerData.DeckGenerators._
import poker.Hand._
import poker.OrderInstances._

import scala.util.Random

object HandGenerators {

  val genStraightFlush: Gen[StraightFlush] = for {
    newSuit  <- genSuit
    straight <- genStraight
  } yield StraightFlush(straight.rank)

  val genFourOfAKind: Gen[FourOfAKind] =
    for {
      rank <- genRank
      quads = Deck.all.groupBy(_.rank)(rank)
      card1 <- genCard.suchThat(c => c.rank != rank)
      card2 <- genCard.suchThat(c => c != card1 && c.rank != rank)
      card3 <- genCard.suchThat(c => !List(card1, card2).contains(c) && c.rank != rank)
      kickers = List(card1, card2, card3).sorted.reverse
    } yield Hand.rank(card1 :: card2 :: card3 :: quads).asInstanceOf[FourOfAKind]

  val genFullHouse: Gen[FullHouse] =
    for {
      (rank1, rank2) <- pick(2, Rank.all).map(x => (x.head, x.last))
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
    } yield Hand.rank(card1 :: card2 :: pair.toList ++ set.toList).asInstanceOf[FullHouse]

  val genFlush: Gen[Flush] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    flush <- pick(5, suited).suchThat(x =>
      Hand.rank(x.toList) match {
        case _: StraightFlush => false
        case _                => true
      }
    )
    card1 <- genCard.suchThat { c =>
      !suited.contains(c)
    }
    card2 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1
    }
  } yield Hand.rank(card1 :: card2 :: flush.toList).asInstanceOf[Flush]

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
  } yield Hand.rank(card1 :: card2 :: card3 :: fourFlush.toList)

  val genWheelStraight: Gen[Straight] = {
    val wheelRanks = List(Ace, Five, Four, Three, Two)
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.suchThat(s => List(suit1, suit2, suit3, suit4).count(_ == s) < 4)
      suits = List(suit1, suit2, suit3, suit4, suit5)
    } yield Hand.rank(wheelRanks.zip(suits).map(x => Card(x._1, x._2))).asInstanceOf[Straight]
  }

  def genStraight_(high: Int): Gen[Straight] = {
    val idx                               = choose(0, high).sample.get
    val grouped: List[(Rank, List[Card])] = Deck.all.groupBy(_.rank).toList.sortBy(_._1)
    val hslice: List[(Rank, List[Card])]  = grouped.slice(idx, idx + 5)
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.retryUntil(s => List(suit1, suit2, suit3, suit4).count(_ == s) < 4)
      suit6 <- genSuit.retryUntil(s => List(suit1, suit2, suit3, suit4, suit5).count(_ == s) < 4)
      suit7 <- genSuit.retryUntil(s => List(suit1, suit2, suit3, suit4, suit5, suit6).count(_ == s) < 4)
      c1    <- Gen.oneOf(hslice.head._2)
      c2    <- Gen.oneOf(hslice(1)._2)
      c3    <- Gen.oneOf(hslice(2)._2)
      c4    <- Gen.oneOf(hslice(3)._2)
      c5    <- Gen.oneOf(hslice(4)._2)
      n1    <- genCard.retryUntil(c => !List(c1, c2, c3, c4, c5).contains(c))
      n2    <- genCard.retryUntil(c => !List(c1, c2, c3, c4, c5, n1).contains(c))
    } yield Hand
      .rank(
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
      .asInstanceOf[Straight]
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

  val genThreeOfAKind: Gen[ThreeOfAKind] = for {
    ranks <- pick(5, Rank.all).retryUntil { r =>
      val x = r.toList.map(_.value).sorted
      x.head + 4 != x(4) && x.head + 12 != x(4)
    }
    suits <- pick(3, Suit.all)
    set = List(Card(ranks.head, suits.head), Card(ranks.head, suits(1)), Card(ranks.head, suits(2)))
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
  } yield Hand.rank(card1 :: card2 :: card3 :: card4 :: set).asInstanceOf[ThreeOfAKind]

  val genTwoPair: Gen[TwoPair] = for {
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
        (Hand.rank(card1 :: card2 :: c :: pair1.toList ++ pair2.toList) match {
          case _: Straight => false
          case _           => true
        }) //TODO again
    )
  } yield Hand.rank(card1 :: card2 :: card3 :: pair1.toList ++ pair2.toList).asInstanceOf[TwoPair]

  val genPair: Gen[Pair] = for {
    rank <- genRank
    grouped: Map[Rank, List[Card]] = Deck.all.groupBy(_.rank)
    pair <- pick(2, grouped(rank))
    //    rankingList = HandRank.all.filterNot(_ == Pair)
    rest <- pick(5, Deck.all.filterNot(pair.contains(_))).retryUntil(x =>
      Hand.rank(pair.toList ++ x.toList) match {
        case _: Pair => true
        case _       => false
      }
    )
  } yield Pair(rank, rest.sorted.reverse.toList)

  val genHighCard: Gen[HighCard] = {
    val suits  = Random.shuffle(Suit.all)
    val offset = choose(0, 3).sample.get
    val hand1 = Hand.rank(
      List(
        Card(rank = rankMap(11 + offset), suit = suits.head),
        Card(rank = rankMap(10 + offset), suit = suits.head),
        Card(rank = rankMap(8 + offset), suit = suits(1)),
        Card(rank = rankMap(7 + offset), suit = suits(1)),
        Card(rank = rankMap(5 + offset), suit = suits(2)),
        Card(rank = rankMap(4 + offset), suit = suits(2)),
        Card(rank = rankMap(2 + offset), suit = suits(3))
      )
    )

    val hand2 = Hand
      .rank(
        List(
          Card(rankMap(11 + offset), suits.head),
          Card(rankMap(9 + offset), suits.head),
          Card(rankMap(8 + offset), suits(1)),
          Card(rankMap(6 + offset), suits(1)),
          Card(rankMap(5 + offset), suits(2)),
          Card(rankMap(3 + offset), suits(2)),
          Card(rankMap(2 + offset), suits(3))
        )
      )

    val hand3 = Hand
      .rank(
        List(
          Card(rankMap(10 + offset), suits.head),
          Card(rankMap(9 + offset), suits.head),
          Card(rankMap(8 + offset), suits(1)),
          Card(rankMap(7 + offset), suits(1)),
          Card(rankMap(5 + offset), suits(2)),
          Card(rankMap(4 + offset), suits(2)),
          Card(rankMap(2 + offset), suits(3))
        )
      )
// TODO Ask about this one
    val variations = List(hand1, hand2, hand3).map(_.asInstanceOf[HighCard])
    oneOf(variations)
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
