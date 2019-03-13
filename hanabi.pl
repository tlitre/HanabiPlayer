%% Cards are of format (Suite, Value, Count)
deck(Cards) :-
    findall(card(S,1,C), (between(1,5,S),between(1,3,C)), C1),
    findall(card(S,V,C), (between(1,5,S),between(2,4,V),between(1,2,C)), C234),
    findall(card(S,5,1), (between(1,5,S)), C5),
    append(C1,C234,C1234),
    append(C1234,C5,Cards).

shuffle(Cards, Random_Cards) :-
    random_permutation(Cards,Random_Cards).

%% helpful function taken from stack overflow
%% the first N elements of the second argument are the third argument
take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

%% function defined as a complement to that above
%% all but the first N elements of the second argument are the third argument
drop(N, Xs, Ys) :- N =< 0, !, N =:= 0, Ys = Xs.
drop(_, [], []).
drop(N, [_|Xs], Ys) :- M is N-1, drop(M, Xs, Ys).

first_deal(Cards, Player_Hand, Opponent_Hand, Remaining_Cards) :-
    take(5,Cards,Player_Hand),
    drop(5,Cards,CardsToDeal),
    take(5,CardsToDeal,Opponent_Hand),
    drop(5,CardsToDeal,Remaining_Cards).

%% Set board to empty
%% setup_board(Board)
setup_board([0,0,0,0,0]).

%% Check if board has been completed
%% is_board_complete(Board)
is_board_complete([5,5,5,5,5]).

%% See if a card can be played without losing a fuse token
%% is_card_playable(Card,Board)
is_card_playable(card(1,V,_), [Bd|_]) :-
    V =:= Bd+1.
is_card_playable(card(S,V,_), [_|Bd]) :-
    S > 1,
    M is S-1,
    is_card_playable(card(M,V,_),Bd).

%% play_card_to_board(Card, Board, New_Board)
play_card_to_board(card(1,V,_), [Bd|Remaining_Board], [V|Remaining_Board]) :-
    V =:= Bd+1.
play_card_to_board(card(S,V,_), [Bd|Remaining_Board], [Bd|New_Board]) :- 
    S > 1,
    M is S-1,
    play_card_to_board(card(M,V,_),Remaining_Board,New_Board).

%% get nth card from a hand
%% get_card_from_hand(N,Hand,Card_From_Hand)
get_card_from_hand(1, [Card|_], Card).
get_card_from_hand(N, [_|Hand], Card) :- M is N-1, get_card_from_hand(M,Hand,Card).

%% get nth card from a hand and form remaining cards into a new hand
%% remove_card_from_hand(N, Hand, Card, Remaining_Hand)
remove_card_from_hand(1, [Card|Remaining_Hand], Card, Remaining_Hand).
remove_card_from_hand(N, [Card0|Hand], Card, [Card0|Remaining_Hand]) :- 
    M is N-1,
    remove_card_from_hand(M,Hand,Card, Remaining_Hand).

%% draw a card and add it to player hand.
%% draw_card(Deck, Player_Hand, Remaining_Deck, New_Player_Hand)
draw_card([Card|Remaining_Cards],Player_Hand,Remaining_Cards,[Card|Player_Hand]).

%% traces the game and prints game steps
tracer([]).
tracer(["nl"|Xs]) :-
    nl,
    tracer(Xs).

tracer([X|Xs]) :-
    write(X),
    tracer(Xs).

%% Play a round by discarding the nth card in hand
%% play_discard(N, Deck, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge)

play_discard(N, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    I is Information_Tokens+1,
    remove_card_from_hand(N, Player_Hand, Card, Remaining_Hand),
    remove_card_from_hand(N, Player_Knowledge, _, New_Player_Knowledge),
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
    play_round(Cards, [Card|Discard_Pile], Opponent_Hand, Remaining_Hand, Board, Fuse_Tokens, I, Opponent_Knowledge, New_Player_Knowledge).

%% play a round by playing nth card
%% play_card(Deck, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge)
play_card(N, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    remove_card_from_hand(N, Player_Hand, Card, Remaining_Hand),
    is_card_playable(Card, Board),
    play_card_to_board(Card,Board,New_Board),
    remove_card_from_hand(N, Player_Knowledge, _, New_Player_Knowledge),
    write("Playing Card:"),
    write(Card),
    nl,
    wrtie("Board:"),
    nl,
    write(New_Board),
    nl,
    !,
    play_round(Cards, Discard_Pile, Opponent_Hand,Remaining_Hand,New_Board,Fuse_Tokens,Information_Tokens,Opponent_Knowledge,New_Player_Knowledge).

play_card(N, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    remove_card_from_hand(N, Player_Hand, Card, Remaining_Hand),
    \+ is_card_playable(Card, Board),
    F is Fuse_Tokens-1,
    remove_card_from_hand(N, Player_Knowledge, _, New_Player_Knowledge),
    write("Playing Card: "),
    write(Card),
    nl,
    write("Board:"),
    nl,
    write(Board),
    nl,
    write("Fuse Token lost!"),
    nl,
    !,
    play_round(Cards, [Card|Discard_Pile], Opponent_Hand,Remaining_Hand,Board,F,Information_Tokens,Opponent_Knowledge, New_Player_Knowledge).

%% return the card Suite
%% get_card_color(Card, Color)
get_card_color(card(S,_,_), S).

%% check if the colors of hand cards match color
%% match_colors(Color, Hand, Matched_Colors)
match_colors(_,[], []).

match_colors(Color,[card(Color,_,_)|Rest],[Color|RestAcc]) :-
    match_colors(Color, Rest, RestAcc).

match_colors(Color, [card(S,_,_)|Rest],[0|RestAcc]) :-
    S \= Color,
    match_colors(Color, Rest, RestAcc).

%% return the card value
%% get_card_value(Card, Value)
get_card_value(card(_,V,_), V).

%% check if the values of hand cards match value
%% match_values(Value, Hand, Matched_Values)
match_values(_,[], []).

match_values(Value,[card(_,Value,_)|Rest],[ValueShifted|RestAcc]) :-
    ValueShifted is Value*10,
    match_values(Value, Rest, RestAcc).

match_values(Value, [card(_,V,_)|Rest],[0|RestAcc]) :-
    V \= Value,
    match_values(Value, Rest, RestAcc).

%% maps 3rd argument to the logical or of first two arguments
%% match_knowledge(New_Information, Existing_Knowledge, Updated_Knowledge)
match_knowledge([],[],[]).

match_knowledge([A|As],[B|Bs], [C|RestAcc]) :-
    C is A \/ B,
    match_knowledge(As,Bs,RestAcc).

%% play an information token to provide knowledge to other player
%% in this case, provide information on the color of the nth card
%% play_inform_color(N, Deck, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge)
play_inform_color(N, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    I is Information_Tokens-1,
    get_card_from_hand(N, Opponent_Hand, Card),
    get_card_color(Card,Color),
    match_colors(Color,Opponent_Hand,Matched_Colors),
    match_knowledge(Matched_Colors,Opponent_Knowledge,New_Opponent_Knowledge),
    write("Giving color knowledge to opponent:"),
    nl,
    write(Matched_Colors),
    nl,
    !,
    play_round(Cards, Discard_Pile, Opponent_Hand, Player_Hand, Board, Fuse_Tokens, I, New_Opponent_Knowledge, Player_Knowledge).

%% play an information token to provide knowledge to other player
%% play_inform_value(N,Deck,Discard_Pile,Player_Hand,Opponent_Hand,Board,Fuse_Tokens,Information_Tokens,Player_Knowledge,Opponent_Knowledge)
play_inform_value(N, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    I is Information_Tokens-1,
    get_card_from_hand(N, Opponent_Hand, Card),
    get_card_value(Card,Value),
    match_values(Value,Opponent_Hand,Matched_Values),
    match_knowledge(Matched_Values,Opponent_Knowledge,New_Opponent_Knowledge),
    write("Giving value knowledge to opponent:"),
    nl,
    write(Matched_Values),
    nl,
    !,
    play_round(Cards, Discard_Pile, Opponent_Hand, Player_Hand, Board, Fuse_Tokens, I, New_Opponent_Knowledge, Player_Knowledge).


%%read input when playing with a human
get_human_input(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    write('Your current hand:'),
    write(Opponent_Hand),
    nl,
    write('Your move! d to discard, p to play, or c to give a clue'),
    nl,
    read(Move),
    play_human_move(Move, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).

%%when human player chooses to discard 
play_human_move(d, Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    write('Which card to discard? Answer 1, 2, 3, or 4.'),
    read(D),
    I is Information_Tokens+1,
    remove_card_from_hand(D, Opponent_Hand, Card, Remaining_Hand),
    write("Discarding card: "), 
    write(Card), 
    nl, 
    write("Board: "), 
    nl, 
    write(Board), 
    nl,
    write("Information Tokens remaining:"),
    I,
    nl.

%%human player chooses to play a card
%%%implemented this by switching the opponent and player hand, not sure if I can do that. will test.
play_human_move(p, Cards, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    write('Which card to play? Answer 1, 2, 3, or 4.'),
    read(P),
    !,
    play_card(P, Cards, Opponent_Hand, Player_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).

%%TODO: Implement Human Player Gives Clue


%% Scoring Function
score_board([], 0).
score_board([Bd|RestBd],Score) :- Score =:= Bd + RestScore, score_board(RestBd,RestScore).

%% Play the Game
%% play_round(Deck, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge)

%% End the game if all fuse tokens are used.
play_round(_,_,_,_,Board,0,_,_,_) :-
    write("Out of fuse tokens! Board:"),
    nl,
    write(Board),
    nl.

play_round([],_,[],_,Board,_,_,_,_) :-
    write("Out of cards! Board:"),
    nl,
    write(Board),
    nl.
    
%% if player has fewer than 5 cards, draw a card and continue play
play_round(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
    length(Player_Hand, 4),
    length(Cards, N),
    N > 0,
    draw_card(Cards, Player_Hand, Remaining_Cards, New_Player_Hand),
    write("Drawing card..."),
    nl,
    !,
    play_round(Remaining_Cards, Discard_Pile, New_Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, [0|Player_Knowledge], Opponent_Knowledge).
 

%% test agent 4: play game by spending all information tokens, then discarding cards
play_round(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
   Information_Tokens \= 0,
   play_inform_value(1,Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);
   Information_Tokens \= 0,
   play_inform_color(1,Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);
   play_discard(1,Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).


%% test agent 3: play game by spending all information tokens, then discarding cards
%% play_round(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
%%   Information_Tokens \= 0,
%%    play_inform_color(1,Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge);
%%   play_discard(1,Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).

%% test agent 2: play game by playing first card
%% play_round(Cards, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
%%    play_card(1,Cards, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).

%% test agent 1: play game by always discarding first card
%% play_round(Cards, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge) :-
%%    play_discard(1,Cards, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).


play_game() :-
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
    play_round(Remaining_Cards, [], Player_Hand, Opponent_Hand, Board, 3, 8, [0,0,0,0,0], [0,0,0,0,0]),
    %get_human_input(Remaining_Cards, [], Player_Hand, Opponent_Hand, Board, 3, 8, [0,0,0,0,0], [0,0,0,0,0]), 
    write("Game Over."),
    nl.
