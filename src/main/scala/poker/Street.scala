package poker

case class Street() {
  // players/players hands from 2 - 9
  // board cards 0 - 5
}
case object Preflop extends Street
case object Flop    extends Street
case object Turn    extends Street
case object River   extends Street
