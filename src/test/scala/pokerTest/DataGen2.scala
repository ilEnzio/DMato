package pokerTest

import cats.effect.unsafe.implicits.global
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{choose, oneOf, pick}
import poker.BoardState.{Flop, Preflop, Turn}
import poker.Deck.{all, makeStartingDeck}
import poker.Hand.{
  Flush,
  FourOfAKind,
  FullHouse,
  HighCard,
  Pair,
  Straight,
  StraightFlush,
  ThreeOfAKind,
  TwoPair,
  UnrankedHand
}
import poker.Rank.rankMap
import poker.{Ace, Card, Deck, Five, Four, Hand, Jack, King, Player, Queen, Rank, Suit, Ten, Three, Two}
import poker.OrderInstances._

import scala.util.Random

object DataGen2 {

  val genSuit: Gen[Suit] = Gen.oneOf(Suit.all)
  val genRank: Gen[Rank] = Gen.oneOf(Rank.all)
  val genCard: Gen[Card] = for {
    rank <- genRank
    suit <- genSuit
  } yield Card(rank, suit)

  val startingDeck: Deck = makeStartingDeck //

  val genDeck: Gen[Deck] = {
    println(makeStartingDeck.shuffle.unsafeRunSync().cards)
    Gen.const(makeStartingDeck)
  }

  implicit val arbCard: Arbitrary[Card] = Arbitrary(genCard)
  //  implicit val arbDeck = Arbitrary(startingDeck)

  val genHand: Gen[UnrankedHand] = for {
    cards <- pick(7, all)
  } yield UnrankedHand(cards.toList)

  implicit val arbHand: Arbitrary[Hand] = Arbitrary(genHand)
  implicit val arbRank: Arbitrary[Rank] = Arbitrary(genRank)

  // Optimal Output Generators

  // these generators are severely lacking.

  val genHighCard: Gen[HighCard] = {
    val suits  = Random.shuffle(Suit.all)
    val offset = choose(0, 3).sample.get
    val hand1List = List(
      Card(rank = rankMap(11 + offset), suit = suits.head),
      Card(rank = rankMap(10 + offset), suit = suits.head),
      Card(rank = rankMap(8 + offset), suit = suits(1)),
      Card(rank = rankMap(7 + offset), suit = suits(1)),
      Card(rank = rankMap(5 + offset), suit = suits(2)),
      Card(rank = rankMap(4 + offset), suit = suits(2)),
      Card(rank = rankMap(2 + offset), suit = suits(3))
    )
    val hand1: HighCard = Hand.rank(hand1List).asInstanceOf[HighCard]

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
      .asInstanceOf[HighCard]

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
      .asInstanceOf[HighCard]

    val variations = List(hand1, hand2, hand3)
    oneOf(variations)
  }

  val genAceHigh: Gen[HighCard] = {
    for {
      hand <- genHighCard
      suit <- genSuit
    } yield Hand.rank(Card(Ace, suit) :: hand.cards.sorted.reverse.tail).asInstanceOf[HighCard]
  }

  val genFourOfAKind: Gen[FourOfAKind] =
    for {
      rank <- genRank
      quads = all.groupBy(_.rank)(rank)
      card1 <- genCard.suchThat(c => c.rank != rank)
      card2 <- genCard.suchThat(c => c != card1 && c.rank != rank)
      card3 <- genCard.suchThat(c => !List(card1, card2).contains(c) && c.rank != rank)
    } yield Hand.rank(card1 :: card2 :: card3 :: quads).asInstanceOf[FourOfAKind]

  val genFullHouse: Gen[FullHouse] =
    for {
      rank1 <- genRank
      rank2 <- genRank.suchThat(r => r != rank1)
      grouped = all.groupBy(_.rank)
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

  val genDeucesFullOfTres: Gen[FullHouse] = {
    val rank1   = Two
    val rank2   = Three
    val grouped = all.groupBy(_.rank)

    for {
      set  <- pick(3, grouped(rank1))
      pair <- pick(2, grouped(rank2))
    } yield Hand.rank(pair.toList ++ set.toList).asInstanceOf[FullHouse]
  }

  val genFlush: Gen[Flush] = for {
    suit <- genSuit
    suited = all.filter(_.suit == suit)
    flush <- pick(5, suited)
    card1 <- genCard.suchThat { c =>
      !suited.contains(c)
    }
    card2 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1
    }
  } yield Hand.rank(card1 :: card2 :: flush.toList).asInstanceOf[Flush]

  val genNutFlush: Gen[Flush] = for {
    suit <- genSuit
    suited = all.filter(_.suit == suit)
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
    suited = all.filter(_.suit == suit)
    flush <- pick(5, suited).retryUntil(x =>
      !x.contains(Card(Ace, suit)) &&
        (Hand.rank(x.toList) match {
          case _: StraightFlush => false
          case _                => true
        }) // todo Again...???
    )
  } yield Hand.rank(flush.toList).asInstanceOf[Flush]

  val genNonFlush: Gen[Flush] = for {
    suit <- genSuit
    suited = all.filter(_.suit == suit)
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
  } yield Hand.rank(card1 :: card2 :: card3 :: fourFlush.toList).asInstanceOf[Flush]

  /// group card by rank; pick a rank between A-5
  // take a card from that top rank and 4 ranks beneath it.
  // 5 is a special case
  // 0-2, 1-3,2-4,3-5,4-6,5-7,6-8,7-9,8-10,9-11,10-12,11-12,12-14
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

  def genNutStraight: Gen[Straight] = {
    val broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    for {
      suit1 <- genSuit
      suit2 <- genSuit
      suit3 <- genSuit
      suit4 <- genSuit
      suit5 <- genSuit.suchThat(s => List(suit1, suit2, suit3, suit4).count(_ == s) < 4)
      suits = List(suit1, suit2, suit3, suit4, suit5)
    } yield Hand.rank(broadWayRanks.zip(suits).map(x => Card(x._1, x._2))).asInstanceOf[Straight]
  }

  val genNonWheelStraight: Gen[Straight] =
    genStraight_(8)

  val genStraight: Gen[Straight] = {
    for {
      hand1     <- genWheelStraight
      hand2     <- genNonWheelStraight
      finalHand <- oneOf(List(hand1, hand2))
    } yield finalHand
  }

  //  val genNonNutStraight: Gen[Hand] = // broken
  //    genStraight_(7)

  def genStraight_(high: Int): Gen[Straight] = {
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
      c1    <- Gen.oneOf(hslice.head._2)
      c2    <- Gen.oneOf(hslice(1)._2)
      c3    <- Gen.oneOf(hslice(2)._2)
      c4    <- Gen.oneOf(hslice(3)._2)
      c5    <- Gen.oneOf(hslice(4)._2)
      n1    <- genCard.suchThat(c => !List(c1, c2, c3, c4, c5).contains(c))
      n2    <- genCard.suchThat(c => !List(c1, c2, c3, c4, c5, n1).contains(c))
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
    grouped = all.groupBy(_.rank)
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
    grouped: Map[Rank, List[Card]] = all.groupBy(_.rank)
    pair <- pick(2, grouped(rank))
    //    rankingList = HandRank.all.filterNot(_ == Pair)
    rest <- pick(5, all.filterNot(pair.toList.contains(_))).retryUntil(x =>
      Hand.rank(pair.toList ++ x.toList) match {
        case _: Pair => true
        case _       => false
      }
    )
  } yield Hand
    .rank(pair.toList ++ rest.toList)
    .asInstanceOf[Pair] // Hand_2.rank(pair.toList ++ hand.cards.take(5))[Pair]

  val genStraightFlush: Gen[StraightFlush] = for {
    newSuit  <- genSuit
    straight <- genStraight
  } yield Hand.rank(straight.cards.distinctBy(_.rank).map(x => x.copy(suit = newSuit))).asInstanceOf[StraightFlush]

  val genNutStraightFlush: Gen[StraightFlush] = {
    val broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    for {
      suit <- genSuit
      suitList = List.fill(5)(suit)
    } yield Hand.rank(broadWayRanks.zip(suitList).map(x => Card(x._1, x._2))).asInstanceOf[StraightFlush]
  }

  val genNutStraightFlush2: Gen[StraightFlush] = {

    for {
      broadway <- genNutStraight
      newSuit  <- genSuit
    } yield Hand.rank(broadway.cards.map(c => c.copy(suit = newSuit))).asInstanceOf[StraightFlush]
  }

  val genNonNutStraightFlush: Gen[StraightFlush] = for {
    newSuit  <- genSuit
    straight <- genStraight.suchThat(x => !x.cards.map(_.rank).contains(Ace))
    cards = straight.cards.distinct
  } yield Hand.rank(cards.map(_.copy(suit = newSuit))).asInstanceOf[StraightFlush]

  val genPreflop: Gen[Preflop] = {
    // Deck
    // 2-9 players
    // for each player take 2 cards from the deck
    val numPlayers = choose(2, 9).sample.get
    val deck       = makeStartingDeck.shuffle.unsafeRunSync()
    val cards      = deck.take(numPlayers * 2)
    val newDeck    = deck.drop(numPlayers * 2)
    // TODO still not safe!!
    val players = cards.grouped(2).map { case h :: t => Player(h, t.headOption.get) }.toList

    Preflop(players, newDeck)
  }

  val genFlop: Gen[Flop] = {
    for {
      preflop <- genPreflop
      newDeck     = preflop.deck.drop(3)
      f :: s :: t = preflop.deck.take(3)
    } yield Flop(preflop.players, newDeck, f, s, t.headOption.get)

  }

  val genTurn: Gen[Turn] = {
    for {
      flop <- genFlop
      newDeck = flop.deck.drop(1)
      turn    = flop.deck.take(1)
    } yield Turn(flop.players, newDeck, flop.card1, flop.card2, flop.card3, turn.headOption.get)

  }

}
