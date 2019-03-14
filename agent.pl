%% include the main file
:- consult(hanabi).

%% leftmost card in hand is the newest, rightmost unmarked is the chop
%% always play your leftmost playable card
%% always discard the chop if discarding
%% every clue leads to playable card(s) or stopping important discards

%% Check to see if opponent has playable cards
%% Opponent's Hand, Board, List of Playable Cards in Opponent's Hand
get_opponent_playables([], _, _).

get_opponent_playables([A|Rest], Board, Playables) :-
    is_card_playable(A, Board),
    opponent_playabables(Rest, Board, [A|Playables]).

get_opponent_playables([A|Rest], Board, Playables) :-
    \+ is_card_playable(A, Board),
    opponent_playabables(Rest, Board, Playables).

%get_opponent_playables(Opponent_Hand, Board, Playables) :-
%   include(is_card_playable(Board), Opponent_Hand, Playables).

%%Gets card knowledge (can do it by card or by index)
get_card_knowledge(Ind, Card, Hand, Hand_Knowledge, Card_Knowledge) :-
    nth0(Ind, Hand, Card),
    nth0(Ind, Hand_Knowledge, Card_Knowledge).

%% Does opponent know they can play a card for next turn
%% Opponent_Hand, Opponent_Knowledge, Playables
opponent_knows_play(Opponent_Hand, Opponent_Knowledge, Playables) :-
    member(Card, Playables),
    get_card_knowledge(_, Card, Opponent_Hand, Opponent_Knowledge, Card_Knowledge),
    Card_Knowledge > 0. 

%%Finds the rightmost unknown card (needs to be passed the hand and knowledge in reverse)

%% TODO change this to check if any are not playable
%% Failure state in case we know something about every card, pick the oldest one.
get_chop(Hand, Knowledge, Chop) :-
    reverse(Hand, Reversed_Hand),
    reverse(Knowledge, Reversed_Knowledge),
    get_reversed_chop(Reversed_Hand, Reversed_Knowledge, Chop),
    , !.

get_reversed_chop([Chop|_], [_|[]], [Card_Knowledge|[]], Chop) :-
    Card_Knowledge > 0, !.

get_reversed_chop(_, [Card|_], [Card_Knowledge|_], Card) :-
    Card_Knowledge = 0, !.

get_reversed_chop(Reversed_Hand, [Card|Rest_Hand], [Card_Knowledge|Rest_Knowledge], Card) :-
    Card_Knowledge > 0,
    get_reversed_chop(Reversed_Hand, Rest_Hand, Rest_Knowledge, _).

%% Card is the only one of its kind (color/value combo)

card_is_unique(card(_,5,_), _).

card_is_unique(card(Color,Value,_), Discard_Pile) :-
   total_in_deck(Value, Total),
   findall(I, has_card(card(Color,Value,_), Discard_Pile), L),
   length(L, Count),
   Total - Count <= 1, !.

%% TODO check if any cards below it have all been discarded
card_is_useless(card(Color,Value,_) Board) :-
    I is Color + 1,
    nth0(I, Board, Score),
    Score >= Value, !.
    
%% Check if the card is not discardable
card_is_important(Card, Discard_Pile, Board) :-
    +\ card_is_useless(Card, Board),
    card_is_unique(Card, Discard_Pile), !.

value_possibly_playable(Value, Board) :-
    V is Value - 1,
    member(card(_, V, _), Board),!.

suite_possible_playable(Value, Board) :-
    S is Suit - 1,
    nth0(S, Board, Score),
    Score < 5, !.
    

%% TODO
%% Figure out if we know we have a playable card
% The case where we have full information about a card.

get_own_playable(_, 6, _) :-
    false.

get_own_playable(Player_Knowledge, Left_Playable, Board) :-
    get_value_from_knowledge(Left_Playable, Player_Knowledge, Value),  
    get_suite_from_knowledge(Left_Playable, Player_Knowledge, Suite),
    Suite > 0,
    Value > 0,
    is_card_playable(card(Suite, Value, _), Board), !.

get_own_playable(Player_Knowledge, Left_Playable, Board) :- 
    get_value_from_knowledge(Left_Playable, Player_Knowledge, Value),  
    get_suite_from_knowledge(Left_Playable, Player_Knowledge, Suite),
    Value > 0,
    Suite = 0,
    value_possibly_playable(Value, Board), ! ;

    get_value_from_knowledge(Left_Playable, Player_Knowledge, Value),  
    get_suite_from_knowledge(Left_Playable, Player_Knowledge, Suite),
    Value = 0,
    Suite > 0,
    suit_possibly_playable(Value, Board), ! ;

    Incr is Left_Playable + 1,
    get_own_playable(Player_Knowledge, Incr, Board), !.



%% flowchart for agent decisions:
%% check that giving a clue is possible (we have information tokens)
%% get opponent's playable cards
%% check if opponent knows something about a playable card
%% if not:
%%      check that their next chop is not an important card:
%%          if yes:
%%              tell them about it
%%          if not:
%%              check if I have an immediately playable move
%%                  if yes:
%%                      play it
%%                  if not:
%%                      if opponent has playable card they don't know about it, tell them
%%                      else, discard

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%  MAIN REASONER  %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Player Hand is included for control flow purposes but cannot be used in reasoning

%%TODO implement __decisions functions (talking to game representation)

%% no info tokens, agent no playables
agent_reasoner(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    Information_Tokens = 0,
    +\ get_own_playable(Player_Knowledge, Left_Playable, Board),
    get_chop(Player_Hand, Player_Knowledge, Chop),
    discard_card_decision(Chop), !.

%% opponent has a move, agent no playables

agent_reasoner(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    Information_Tokens > 0,
    get_opponent_playables(Opponent_Hand, Board, Playables),
    opponent_knows_play(Opponent_Hand, Opponent_Knowledge, Playables), 
    get_own_playable(Player_Knowledge, Left_Playable, Board),
    play_card_decision(Left_Playable), !.

%% case where we end up playing a card
agent_reasoner(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    Information_Tokens > 0,
    get_opponent_playables(Opponent_Hand, Board, Playables),
    \+ opponent_knows_play(Opponent_Hand, Opponent_Knowledge, Playables),
    get_chop(Opponent_Hand, Opponent_Knowledge, Chop),
    \+ card_is_important(Chop, Discard_Pile, Board),
    get_own_playable(Player_Knowledge, Left_Playable, Board),
    play_card_decision(Left_Playable), !.




%% Helpers

total_in_deck(5, N) :-
    N is 1.

total_in_deck(1, N) :-
    N is 3.

total_in_deck(Val, N) :-
    Val < 5,
    Val > 1,
    N is 2.

has_card([Color, Value, _], Ls) :-
    member([Color, Value, _], Ls).

    


