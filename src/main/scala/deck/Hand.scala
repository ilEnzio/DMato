package deck

case class Hand(
  cards: List[Card],
  ranking: Ranking // TODO: Should this be optional?  Then ranked later?
) {}
