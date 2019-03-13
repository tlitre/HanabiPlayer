%% leftmost card in hand is the newest, rightmost unmarked is the chop
%% always play your leftmost playable card
%% always discard the chop if discarding
%% every clue leads to playable card(s) or stopping important discards

%% Check to see if opponent has playable cards
%% Opponent's Hand, Board, List of Playable Cards in Opponent's Hand
get_opponent_playables([], Board, Playables).

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
    get_card_knowledge(Ind, Card, Opponent_Hand, Card_Knowledge),
    Card_Knowledge > 0. 

%%Finds the rightmost unknown card (needs to be passed the hand and knowledge in reverse)

%% TODO change this to check if any are not playable
%% Failure state in case we know something about every card, pick the oldest one.
get_chop(Hand, Knowledge, Chop_Index) :-
    reverse(Hand, Reversed_Hand),
    reverse(Knowledge, Reversed_Knowledge),
    get_reversed_chop(Reversed_Hand, Reversed_Knowledge, Chop),
    nth0(Chop_Index, Hand, Chop), !.

get_reversed_chop([Chop|Reversed_Hand], [Card|[]], [Card_Knowledge|[]], Chop) :-
    Card_Knowledge > 0, !.

get_reversed_chop(Reversed_Hand, [Card|Rest_Hand], [Card_Knowledge|Rest_Knowledge], Card) :-
    Card_Knowledge = 0, !.

get_reversed_chop(Reversed_Hand, [Card|Rest_Hand], [Card_Knowledge|Rest_Knowledge], Card) :-
    Card_Knowledge > 0,
    get_reversed_chop(Reversed_Hand, Rest_Hand, Rest_Knowledge, _).

is_card_important(Discard_Pile, 

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
agent_reasoner(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    Information_Tokens > 0,
    get_opponent_playables(Opponent_Hand, Board, Playables),
    \+ opponent_knows_play(Opponent_Hand, Opponent_Knowledge, Playables),
    get_chop(Opponent_Hand, Opponent_Knowledge, Chop_Index),

    

    


