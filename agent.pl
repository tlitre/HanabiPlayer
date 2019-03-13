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
agent_reasoner(Cards, Discard_Pile, Player_Hand, Opponent_Hand, Board, Fuse_Tokens, Information_Tokens, Player_Knowledge, Opponent_Knowledge).

