Some Tasks:



- ~~Create Deck~~
- ~~Generate random hole cards~~
- ~~Shuffle Deck~~
- Name/string repr of Card
- ~~Fix Card model for Ace!~~
- ~~CreateModel for hand ??~~
- create starting hands

- Determine Hand Ranking(winning hand)
  - ~~test for StraightFlush~~
  - ~~test for 4 of kind~~
  - ~~test for boat~~
  - ~~test for flush~~
  - ~~test for straight~~
  - ~~test for set~~
  - ~~test for two pair~~
  - ~~test for pair~~
  - ~~Determine winner from hands of same Ranking~~
    - Showdown.. still goofy 
    - refactor HandRank to be different types that extend hand
    with the intent to make ranking a hand a pipeline.  
  - get rid of List of Cards from Hand_2 model (x, f(x))
  - get rid of UnrankedHand
  - 


- Calculate the Equity between two players
- Handle up to 9 players
- User can select Starting hands

- Start using Cats for better equality

Notes:
- Some effectful areas
  - the inital Deck generation
- Candidates for parallelism:
  - the board runout/simulation
  