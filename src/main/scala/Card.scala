case class Card(rank: Rank, suit: Suit) {}

object Card {
  //ToDO
  def name: String = ???
}

sealed trait Suit
case object Spades extends Suit
case object Clubs extends Suit
case object Diamonds extends Suit
case object Hearts extends Suit

sealed trait Rank {
  val value: Int
  val optValue: Option[Int]
}
case object Ace extends Rank {
  val value = 13
  val optValue = Some(1)
}

case object King extends Rank {
  val value = 12
  val optValue = None
}
case object Queen extends Rank {
  val value = 11
  val optValue = None
}
case object Jack extends Rank {
  val value = 11
  val optValue = None
}
case object Ten extends Rank {
  val value = 10
  val optValue = None
}
case object Nine extends Rank {
  val value = 9
  val optValue = None
}
case object Eight extends Rank {
  val value = 8
  val optValue = None
}
case object Seven extends Rank {
  val value = 7
  val optValue = None
}
case object Six extends Rank {
  val value = 6
  val optValue = None
}
case object Five extends Rank {
  val value = 5
  val optValue = None
}
case object Four extends Rank {
  val value = 4
  val optValue = None
}
case object Three extends Rank {
  val value = 3
  val optValue = None
}
case object Two extends Rank {
  val value = 2
  val optValue = None
}

