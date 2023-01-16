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
      cards = broadWayRanks
        .zip(suitList)
        .map(x => Card(x._1, x._2))
        .sorted
        .reverse
      deck = Deck.all.filterNot(cards.contains(_))
      remaining <- pick(2, deck)
    } yield remaining.toList ++ cards
  }

  val genNutStraightFlush: Gen[StraightFlush] = for {
    cards <- genNutStraightFlushCards
  } yield StraightFlush(cards.drop(2).head.rank)

// ToDO - create

  val genNonNutStraightFlushCards: Gen[List[Card]] = for {
    hand1   <- genWheelStraightCards
    hand2   <- genStraightCards_(King)
    strHand <- frequency((1, hand1), (10, hand2))
    newSuit <- genSuit
    cards = strHand.map(_.copy(suit = newSuit))
    deck = Deck.all
      .filterNot(cards.contains(_))
      .filterNot(_ === Card(Ace, newSuit))
    remaining <- pick(2, deck)
  } yield remaining.toList ++ cards

  val genNonNutStraightFlush: Gen[StraightFlush] = for {
    cards <- genNonNutStraightFlushCards
    /// TODO not comfortable... lot of trust here lol
    rank = cards.drop(2) match {
      case h :: _ if h.rank != Ace => h.rank
      case _                       => Five
    }
  } yield StraightFlush(rank)

  val genTresFullOfDeucesCards: Gen[List[Card]] =
    genFullHouseCards_(Three, Two)

  val genDeucesFullOfTresCards: Gen[List[Card]] =
    genFullHouseCards_(Two, Three)

  val genDeucesFullOfTres: Gen[FullHouse] =
    for {
      cards <- genDeucesFullOfTresCards
    } yield FullHouse(cards.drop(2).head.rank, cards.drop(2).last.rank)

  val genNutFlushCards: Gen[List[Card]] = for {
    suit <- genSuit
    wheelRanks    = List(Ace, Five, Four, Three, Two)
    broadWayRanks = List(Ace, King, Queen, Jack, Ten)
    ranks <- pick(5, Rank.all).suchThat { x =>
      x.contains(Ace) &&
      (x.sorted.reverse != wheelRanks) && (x.sorted.reverse != broadWayRanks)
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

  } yield cards // TODO Flushes don't have kickers

  val genNonNutFlush: Gen[Flush] = for {
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

  val genAceHigh: Gen[HighCard] = {
    for {
      hand <- genAceHighCards
    } yield HighCard(hand.head.rank, hand.tail)
  }

}
