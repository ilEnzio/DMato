package poker

case class Card(rank: Rank, suit: Suit) {}

object Card {
  //TODO
  def name: String =
    // Deck.Ace Spaces = As
    ???

}

sealed trait Suit
object Suit {
  val all: List[Suit] = List(Spades, Clubs, Diamonds, Hearts)
}
case object Spades   extends Suit
case object Clubs    extends Suit
case object Diamonds extends Suit
case object Hearts   extends Suit

sealed trait Rank {
  val value: Int
}
object Rank {

  val rankMap: Map[Int, Rank] = Map(
    14 -> Ace,
    13 -> King,
    12 -> Queen,
    11 -> Jack,
    10 -> Ten,
    9  -> Nine,
    8  -> Eight,
    7  -> Seven,
    6  -> Six,
    5  -> Five,
    4  -> Four,
    3  -> Three,
    2  -> Two
  )

  val all: List[Rank] = List(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two)
}

case object Ace extends Rank {
  val value = 14
}

case object King extends Rank {
  val value = 13
}
case object Queen extends Rank {
  val value = 12
}
case object Jack extends Rank {
  val value = 11
}
case object Ten extends Rank {
  val value = 10
}
case object Nine extends Rank {
  val value = 9
}
case object Eight extends Rank {
  val value = 8
}
case object Seven extends Rank {
  val value = 7
}
case object Six extends Rank {
  val value = 6
}
case object Five extends Rank {
  val value = 5
}
case object Four extends Rank {
  val value = 4
}
case object Three extends Rank {
  val value = 3
}
case object Two extends Rank {
  val value = 2
}
