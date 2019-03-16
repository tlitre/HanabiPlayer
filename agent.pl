%% include the main file
:- consult(hanabi).
:- use_module(library(clpfd)).

%% leftmost card in hand is the newest, rightmost unmarked is the chop
%% always play your leftmost playable card
%% always discard the chop if discarding
%% every clue leads to playable card(s) or stopping important discards

%%%%%%%%%%%%%%%%%%%%%
%%%%%ENTRY POINT%%%%%
%%%%%%%%%%%%%%%%%%%%%
play_clever_game() :-
    deck(Cards),
    shuffle(Cards,Random_Cards),
    first_deal(Random_Cards, Player_Hand, Opponent_Hand, Remaining_Cards),
    setup_board(Board),
    write("Starting player hand:"),
    nl,
    write(Player_Hand),
    nl,
    write("Starting opponent hand:"),
    nl,
    write(Opponent_Hand),
    nl,
    write("Starting Board:"),
    nl,
    write(Board),
    nl,
    !,
    agent_reasoner(Remaining_Cards, [], Player_Hand, Opponent_Hand, Board, 3, 8, [0,0,0,0,0], [0,0,0,0,0]),
    %get_human_input(Remaining_Cards, [], Player_Hand, Opponent_Hand, Board, 3, 8, [0,0,0,0,0], [0,0,0,0,0]),
    write("Game Over."),
    nl.

%% Check to see if opponent has playable cards
%% Opponent's Hand, Board, List of Playable Cards in Opponent's Hand
get_opponent_playables(Opponent_Hand, Board, Playables) :-
    findall(I,
                (member(I, Opponent_Hand),
                is_card_playable(I, Board)),
            Playables),
    nth0(0, Playables, _).

%get_opponent_playables(Opponent_Hand, Board, Playables) :-
%   include(is_card_playable(Board), Opponent_Hand, Playables).

%%Gets card knowledge (can do it by card or by index)
get_card_knowledge(Card, Hand, Hand_Knowledge, Card_Knowledge) :-
    nth0(Ind, Hand, Card),
    nth0(Ind, Hand_Knowledge, Card_Knowledge).

opponent_knows_play([], _, []) :-
    fail, !.

%% Does opponent know they can play a card for next turn
%% Opponent_Hand, Opponent_Knowledge, Playables
opponent_knows_play(Opponent_Hand, Opponent_Knowledge, Playables) :-
    member(Card, Playables),
    get_card_knowledge(Card, Opponent_Hand, Opponent_Knowledge, Card_Knowledge),
    Card_Knowledge > 0, !. 

%%Finds the rightmost unknown card (needs to be passed the hand and knowledge in reverse)


%% Failure state in case we know something about every card, pick the oldest one.
get_chop(Hand, Knowledge, Chop) :-
    reverse(Hand, Reversed_Hand),
    reverse(Knowledge, Reversed_Knowledge),
    get_reversed_chop(Reversed_Hand, Reversed_Knowledge, Chop).

get_reversed_chop([Chop|[]], [Card_Knowledge|[]], Chop) :-
    Card_Knowledge > 0.

get_reversed_chop([Chop|_], [Card_Knowledge|_], Chop) :-
    Card_Knowledge = 0.

get_reversed_chop([_|Rest_Hand], [Card_Knowledge|Rest_Knowledge], Chop) :-
    Card_Knowledge > 0,
    get_reversed_chop(Rest_Hand, Rest_Knowledge, Chop).

%% Card is the only one of its kind (color/value combo)

card_is_unique(card(_,5,_), _).

card_is_unique(card(Color,Value,_), Discard_Pile) :-
   total_in_deck(Value, Total),
   findall(I, has_card(card(Color,Value,I), Discard_Pile), L),
   length(L, Count),
   Total - Count = 1.

card_is_useless(card(Color,Value,_), Board) :-
    I is Color + 1,
    nth0(I, Board, Score),
    Score >= Value.
    
%% Check if the card is not discardable
card_is_important(Card, Discard_Pile, Board) :-
    \+ card_is_useless(Card, Board),
    card_is_unique(Card, Discard_Pile).

value_possibly_playable(Value, Board) :-
    V is Value - 1,
    member(V, Board).

suite_possibly_playable(Suite, Board) :-
    S is Suite - 1,
    nth0(S, Board, Score),
    Score < 5.
    
%% Figure out if we know we have a playable card
% The case where we have full information about a card.
get_own_playable(Player_Knowledge, Left_Playable, Board) :-
    between(1, 5, Left_Playable),
    get_value_from_knowledge(Left_Playable, Player_Knowledge, Value),  
    get_suite_from_knowledge(Left_Playable, Player_Knowledge, Suite),
    Suite > 0,
    Value > 0,
    is_card_playable(card(Suite, Value, _), Board).

%% The cases where we know enough about the card.
get_own_playable(Player_Knowledge, Left_Playable, Board) :- 
    between(1, 5, Left_Playable),
    get_value_from_knowledge(Left_Playable, Player_Knowledge, Value),  
    get_suite_from_knowledge(Left_Playable, Player_Knowledge, Suite),
    Value > 0,
    Suite = 0,
    value_possibly_playable(Value, Board);

    between(1, 5, Left_Playable),
    get_value_from_knowledge(Left_Playable, Player_Knowledge, Value),  
    get_suite_from_knowledge(Left_Playable, Player_Knowledge, Suite),
    Value = 0,
    Suite > 0,
    suite_possibly_playable(Value, Board).

%% tries to find a clue that only touches cards that could be playable later
%% also looks for clues with most playable cards right now
%% decide_best_clue(Opponent_Hand, Playables, Clue, Attribute) 
decide_best_clue(Opponent_Hand, Playables, Clue_Ind, Clue_Attribute) :-
    findall(I, 
                (member(Play, Playables), 
                get_card_color(Play, Color),
                match_colors(Color, Opponent_Hand, Matched_Hand_Colors),
                match_colors(Color, Playables, Matched_Playable_Colors),
                count_pos(Matched_Hand_Colors, A),
                count_pos(Matched_Playable_Colors, B),
                I is A - B),
            LC),
    findall(J, (member(Play, Playables), 
                get_card_value(Play, Value),
                match_values(Value, Opponent_Hand, Matched_Hand_Values),
                match_values(Value, Playables, Matched_Playable_Values),
                count_pos(Matched_Hand_Values, C),
                count_pos(Matched_Playable_Values, D),
                J is C - D),
            LV),
    minimum_at(LC, Color_Min, Color_Ind),
    minimum_at(LV, Value_Min, Value_Ind),
    pick_clue_attribute(Color_Min, Color_Ind, Value_Min, Value_Ind, Clue_Attribute, Playable_Ind),
    nth0(Playable_Ind, Playables, Play_Card),
    nth0(Clue_Ind, Opponent_Hand, Play_Card).


pick_clue_attribute(Color_Min, Color_Ind, Value_Min, Value_Ind, Attribute, Min_Ind) :-
    Color_Min < Value_Min,
    Attribute = color,
    Min_Ind = Color_Ind;

    Color_Min >= Value_Min,
    Attribute = value,
    Min_Ind = Value_Ind.


%% flowchart for agent decisions:
%% check that giving a clue is possible (we have information tokens)
%% get opponent's playable cards
%% check if opponent knows something about a playable card
%% if not:
%%      check that their next chop is not an important card:
%%          if yes:
%%              tell them about it
%%          if not:
%% if yes:      check if I have an immediately playable move
%%                  if yes:
%%                      play it
%%                  if not:
%%                      if opponent has playable card they don't know about it, tell them
%%                      else, discard

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%  MAIN REASONER  %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Player Hand is included for control flow purposes but cannot be used in reasoning

%%%%%%%%%%%
%END CASES%
%%%%%%%%%%%

agent_reasoner(_, _, _, _, Board, 0, _, _, _) :-
    score_board(Board, Score),
    write("Out of fuse tokens! Board:"),
    nl,
    write(Board),
    nl,
    write("Final Score: "),
    write(Score),
    nl, !.

agent_reasoner([], _, _, _, Board, _, _, _, _) :-
    score_board(Board, Score),
    write("Out of cards ! Board:"),
    nl,
    write(Board),
    nl,
    write("Final Score: "),
    write(Score),
    nl, !.


%%%%%%%%%%%%
%CLUE CASES%
%%%%%%%%%%%%

%% opponent has playables but doesn't know
agent_reasoner(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    Information_Tokens > 0,
    get_opponent_playables(Opponent_Hand, Board, Playables),
    \+ opponent_knows_play(Opponent_Hand, Opponent_Knowledge, Playables), 
    decide_best_clue(Opponent_Hand, Playables, Clue_Ind, Clue_Attribute),
    clue_card_decision(Clue_Ind, Clue_Attribute, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);

%% opponent has important chop card
    Information_Tokens > 0, 
    get_chop(Opponent_Hand, Opponent_Knowledge, Opponent_Chop),
    card_is_important(Opponent_Chop, Discard_Pile, Board),
    nth0(Chop_Ind, Opponent_Hand, Opponent_Chop),
    clue_card_decision(Chop_Ind, value, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);

%%%%%%%%%%%%
%PLAY CASES%
%%%%%%%%%%%%

%% opponent's all good, player knows about a playable
    get_own_playable(Player_Knowledge, Left_Playable, Board),
    play_card_decision(Left_Playable, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);


%%%%%%%%%%%%%%%
%DISCARD CASES%
%%%%%%%%%%%%%%%

%% no info tokens, agent no playables
    Information_Tokens = 0,
    \+ get_own_playable(Player_Knowledge, _, Board),
    get_chop(Player_Hand, Player_Knowledge, Chop),
    nth0(Chop_Ind, Opponent_Hand, Chop),
    discard_card_decision(Chop_Ind, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);

%% opponent has a move, agent no playables
    get_opponent_playables(Opponent_Hand, Board, Playables),
    opponent_knows_play(Opponent_Hand, Opponent_Knowledge, Playables), 
    \+ get_own_playable(Player_Knowledge, _, Board),
    get_chop(Player_Hand, Player_Knowledge, Chop),
    nth0(Chop_Ind, Opponent_Hand, Chop), 
    discard_card_decision(Chop_Ind, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);


%% opponent no playables and unimportant discard, agent no playables
   \+ get_opponent_playables(Opponent_Hand, Board, _), 
    get_chop(Opponent_Hand, Opponent_Knowledge, Opponent_Chop),
    \+ card_is_important(Opponent_Chop, Discard_Pile, Board),
    \+ get_own_playable(Player_Knowledge, _, Board),
    get_chop(Player_Hand, Player_Knowledge, Chop), 
    nth0(Chop_Ind, Opponent_Hand, Chop),
    discard_card_decision(Chop_Ind, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);



%if all else fails, discard
    get_chop(Player_Hand, Player_Knowledge, Chop),
    nth0(Chop_Ind, Player_Hand, Chop),
    discard_card_decision(Chop_Ind, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).





%%%%%%%%%%%
%DECISIONS%
%%%%%%%%%%%

%% clue color
clue_card_decision(Clue_Ind, color, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    I is Information_Tokens - 1,
    nth0(Clue_Ind, Opponent_Hand, Card),
    get_card_color(Card, Color),
    match_colors(Color, Opponent_Hand, Matched_Colors),
    match_knowledge(Matched_Colors,Opponent_Knowledge,New_Opponent_Knowledge),
    write("Giving color knowledge to opponent:"),
    nl,
    write(Matched_Colors),
    nl,
    !,
    agent_reasoner(Cards, Discard_Pile, Opponent_Hand, Player_Hand, Board, Fuse_Tokens, I, New_Opponent_Knowledge, Player_Knowledge).

%% clue value
clue_card_decision(Clue_Ind, value, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    I is Information_Tokens - 1,
    write(Clue_Ind), write(' Hand: '), write(Opponent_Hand), nl,
    nth0(Clue_Ind, Opponent_Hand, Card),
    get_card_value(Card, Value),
    match_values(Value, Opponent_Hand, Matched_Values),
    match_knowledge(Matched_Values,Opponent_Knowledge,New_Opponent_Knowledge),
    write("Giving value knowledge to opponent:"),
    nl,
    write(Matched_Values),
    nl,
    !,
    agent_reasoner(Cards, Discard_Pile, Opponent_Hand, Player_Hand, Board, Fuse_Tokens, I, New_Opponent_Knowledge, Player_Knowledge).

discard_card_decision(Discard_Ind, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    I is Information_Tokens+1,
    remove_card_from_hand(Discard_Ind, Player_Hand, Card, Remaining_Hand),
    remove_card_from_hand(Discard_Ind, Player_Knowledge, _, New_Player_Knowledge),
    draw_card(Cards, Remaining_Hand, Remaining_Cards, New_Player_Hand),
    write("Discarding card:"),
    write(Card),
    nl,
    write("Board: "),
    nl,
    write(Board),
    nl,
    write("Information Tokens remaining: "),
    write(I),
    nl,
    !,
    agent_reasoner(Remaining_Cards, [Card|Discard_Pile], Opponent_Hand, New_Player_Hand, Board, Fuse_Tokens, I, Opponent_Knowledge, [0|New_Player_Knowledge]).

%% play
play_card_decision(Play_Ind, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    remove_card_from_hand(Play_Ind, Player_Hand, Card, Remaining_Hand),
    is_card_playable(Card, Board),
    play_card_to_board(Card, Board, New_Board),
    remove_card_from_hand(Play_Ind, Player_Knowledge, _, New_Player_Knowledge),
    draw_card(Cards, Remaining_Hand, Remaining_Cards, New_Player_Hand),
    write("Playing Card:"),
    write(Card),
    nl,
    write("Board:"),
    nl,
    write(New_Board),
    nl,
    !,
    agent_reasoner(Remaining_Cards, Discard_Pile, Opponent_Hand, New_Player_Hand, New_Board, Fuse_Tokens, Information_Tokens, Opponent_Knowledge, [0|New_Player_Knowledge]).

%% Helpers

add_info_token(Tokens, New_Tokens) :-
    Tokens < 8,
    New_Tokens is Tokens + 1.

add_info_tokens(Tokens, New_Tokens) :-
    Tokens >= 8,
    New_Tokens is 8.

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

%%https://stackoverflow.com/questions/32918211/find-the-max-element-and-its-index-in-a-list-prolog
maximum_at(Zs,Max,Pos) :-
   maplist(#>=(Max),Zs),
   nth0(Pos,Zs,Max). 

minimum_at(Zs,Min,Pos) :-
   maplist(#=<(Min),Zs),
   nth0(Pos,Zs,Min). 


%%https://stackoverflow.com/questions/34423535/prolog-count-positive-elems-in-list
count_pos([], 0).
count_pos([E|Es], C) :- E #=< 0,            count_pos(Es, C).
count_pos([E|Es], C) :- E #>  0, C #= C0+1, count_pos(Es, C0).
