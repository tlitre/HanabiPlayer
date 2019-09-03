# HanabiPlayer
[Hanabi](https://en.wikipedia.org/wiki/Hanabi_(card_game)) is a fireworks-themed cooperative card game. Read more about it, and why we chose to tackle an agent for it in this project, [here](https://tlitre.webflow.io/work/hanabi).

Our implementation of a Hanabi-playing agent emulates human-like conventions and reasoning and is written in Prolog.

## Quickstart:

To watch our agents play out the game, consult the agent.pl file in SWI-Prolog and run `play_clever_game.` This agent makes takes several risks and is our fullest implementation of reasoning, however is still a bit buggy.

Our more conservative and polished agent, though not without some quirks, can be run by consulting hanabi.pl and running `play_game.`

## Hanabi.pl

This file contains the core representation of the game and early attempts at creating a reasoning agent.

To run, start SWI-Prolog in the same directory and load the file using `[hanabi].` A game of Hanabi can then be run using `play_game.`.

### Card, Deck, Board, and Hand Representation

A card is represented as `card(S,V,C)` where S is the suite (1 through 5, corresponding to the five colors), V is the value (1 through 5), and C is the count (1 through 3). A deck or hand is represented as a list of cards.

To build a starting deck called `Cards`, try running `deck(Cards).`.

Shuffling a Deck or Hand can be done using `shuffle(Input_Deck, Output_Deck).`.

The first deal of a 2-player game is represented through `first_deal(Input_Deck, Starting_Player_Hand, Starting_Opponent_Hand, Output_Deck).`.

`setup_board(Board).` will set Board to empty, with no cards played. `setup_board(Board).` may alternatively be used to check if Board is empty.

`is_board_complete(Board).` is primarily used to check if Board has been filled, but can also be used to set Board to full.

`is_card_playable(Card, Board).` checks if card Card is the next card in the sequence of its corresponding suite in Board. For example, `is_card_playable(card(1,4,_), [3,_,_,_,_]).` will be true but `is_card_playable(card(1,4,_), [4,_,_,_,_]).` will be false. Note that both Suite and Value are expected, whereas Count is irrelevant.


`is_card_safe_to_discard(Card, Board).` similarly checks if Card is lower in value than the next card in sequence for its suite.

`play_card_to_board(Card,Input_Board,Output_Board).` will play Card to Input_Board, resulting in the final state OutputBoard. For Card and Input_Board, `is_card_playable(Card,Input_Board).` should evaluate to true.

`get_card_from_hand(N, Hand, Card).` is actually a generic function to return the Nth element of a list (Hand) as Card.

`remove_card_from_hand(N, Input_Hand, Card, Output_Hand).` is similar to `get_card_from_hand` except that it also returns the remaining elements of the list as Output_Hand.

`draw_card(Input_Deck, Input_Hand, Output_Deck, Output_Hand).` takes one card off the top of Input_Deck, places it into Input_Hand, and returns the resulting deck and hand as Output_Deck and Output_Hand. It can also be used with reverse arguments to transfer a card from Output_Hand to Output_Deck!

It is best to ignore `tracer`. This was a function used to print a queue of output messages before the use of the cut was adopted. Since adopting the cut, this function is no longer necessary.

`get_card_color(Card, Color).` returns the suite of a card.

`match_colors(Color, Cards, Colors).` will return an array, Colors, based on which Cards share the color Color.

`get_card_value` and `match_values` function similarly to `get_card_color` and `match_colors`.

`score_board(Board,Score).` sets Score to be the score of a Board. It can also sum lists.

### Action Representation

`play_discard(N, Deck, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).` will play a game from the input state onwards, starting by discarding the Nth card from Player_Hand then continuing from the opponent's turn. This increases the amount of information tokens available.

`play_card(N, Deck, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).` will play a game from the input state onwards, starting by trying to play the Nth card from Player_Hand then continuing from the opponent's turn. Note that this may result in the loss of a fuse token.

`play_inform_color(N, Deck, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).` will play a game from the input state onwards, starting by informing the opponent about the color of the Nth card and all similarly colored cards from Opponent_Hand and then continuing from the opponent's turn. This decreases the amount of information tokens available.

`play_inform_value(N, Deck, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).` will play a game from the input state onwards, starting by informing the opponent about the value of the Nth card and all similarly colored cards from Opponent_Hand and then continuing from the opponent's turn. This decreases the amount of information tokens available.

`get_human_input` and `play_human_move` are components of an incomplete mechanism for playing cards according to human input then passing back to the AI.

### Knowledge Representation

Knowledge about card color is represented in two forms, one a number and one a card. The number form is well-suited for partial knowledge. The suit of the card consists of the number mod 10, while the value of the card is represented by the number div 10. An unknown value is treated as 0. Count does not need to be known. In card form, the full value and suite are known and a dummy count is assigned.

`match_knowledge(A,B,C).` will combine the knowledge about cards from A with the knowledge about cards from B into the combined knowledge C. Using the number representation of the cards, this is just the arithmetic or of the two numbers. A, B, and C are all lists of knowledge numbers.

`get_suite_from_knowledge(N, Knowledge, Suite).` will return the suite of the Nth number in Knowledge based or 0 if it is not known.

`get_value_from_knowledge(N, Knowledge, Suite).` will return the value of the Nth number in Knowledge based or 0 if it is not known.

`get_card_from_knowledge(N, Knowlege, Card).` will try to convert the Nth number in Knowledge into a card representation.

`card_should_be_played(N, Knowledge, Board).` will check whether it is known that the Nth card in one's hand can be played.

`card_should_be_discarded(N, Knowledge, Board).` will similarly check if it is known that the Nth card can safely be discarded.

### Early Reasoning Attempts

`try_play_inform_value`, `try_play_inform_color`, `try_play_card`, and `try_play_discard` contain some basic reasoning to try and play the corresponding moves. If successful, play continues from the given input state. Try play card and try play discard are safe moves. Try play inform will try not to duplicate knowledge.

`discard_no_knowledge`, `discard_not_five`, `discard_if_one`, and `discard_unplayable` all represent attempts to reason about how cards should be discarded. If successful, play continues from the given input state.

`play_round` contains reasoning for deciding that the game has ended and reasoning for how to play a move. Currently, this consists of drawing a card if possible, trying to play a safe card, trying to discard a safe card, trying to inform the other player of new knowledge, then trying to discard as a last resort.

`play_game` puts everything together, constructing the Board, Deck, Hands, and Knowledge, then starting the first move.

### CleverAgent

The agent in agent.pl uses a few conventions to some success, scoring as high as 24 in our testing – though it averages in the low teens.

`agent_reasoner` contains the main reasoner, with a series of disjunctions that decide what to do given the current state

`card_is_important` checks whether a card is a 5 or the last left of its kind

`get_opponent_playable` finds playable cards in our opponents hand

`get_own_playable`, `value_possibly_playable`, and `suite_possibly_playable` reason on the knowledge the agent has about its cards, deeming something playable if the information known leads to the card potentially being playable

`decide_best_clue` finds a clue that maximizes the proportion of playable cards touched when giving a clue, deciding on an attribute and a specific clue
