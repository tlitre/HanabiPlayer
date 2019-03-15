# HanabiPlayer
A Hanabi-playing agent that emulates human-like conventions and reasoning in Prolog.

## Hanabi.pl

This file contains the core representation of the game and early attempts at creating a reasoning agent.

To run, start SWI-Prolog in the same directory and load the file using `[hanabi].` A game of Hanabi can then be run using `play_game.`

### Game Representation

A card is represented as `card(S,V,C)` where S is the suite (1 through 5, corresponding to the five colors), V is the value (1 through 5), and C is the count (1 through 3). A deck or hand is represented as a list of cards.

To build a starting deck called `Cards`, try running `deck(Cards).`