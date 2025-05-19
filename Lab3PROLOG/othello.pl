% Tim Rosendahl
% timros-9

/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING> 
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING> 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
%:- ensure_loaded('play.pl'). % COMMENT AWAY THIS LINE WHEN WE WANT TO SUPPLY STUPID.PL WITH TESTBOARDS


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState, 1) :-
	initBoard(InitialState).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 


winner(State, Player):-
	terminal(State),
    flatten(State,Flat),        % We flatten our 'list-of-lists' board
	countScore(Flat, P1, P2),
    compare_scores(State, P1, P2, Player).

compare_scores(_, P1, P2, 1) :-
    P1 < P2.
compare_scores(_, P1, P2, 2) :-
    P2 < P1.

% We simply check all occurences of 1 or 2 in FlatList
countScore(FlatList, P1, P2) :- 
    findall(1, member(1, FlatList), Ones),
    length(Ones, P1),
    findall(1, member(2, FlatList), Twos),
    length(Twos, P2).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State):-
	terminal(State),
	countScore(State, Player1, Player2),
	Player1 = Player2, !.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :-
  moves(1,State,M1), M1 == [n],
  moves(2,State,M2), M2 == [n].


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

opponent(1,2).
opponent(2,1).

direction( 1, 0).  % E
direction(-1, 0).  % W
direction( 0, 1).  % S
direction( 0,-1).  % N
direction( 1, 1).  % SE
direction( 1,-1).  % NE
direction(-1, 1).  % SW
direction(-1,-1).  % NW

in_bounds([PosX,PosY]) :-
    between(0,5,PosX),
    between(0,5,PosY).

sandwich(Plyr, State, [PosX,PosY], DirectionX, DirectionY) :- % Checks if we have any stones that we 'sandwich'. If yes, we try to flip the sandwiched stones
    opponent(Plyr, Opp),
    X1 is PosX + DirectionX,
    Y1 is PosY + DirectionY,
    in_bounds([X1,Y1]),
    get(State, [X1,Y1], Opp),
    X2 is X1 + DirectionX,
    Y2 is Y1 + DirectionY,
    flip_ray(Plyr, State, X2, Y2, DirectionX, DirectionY).

flip_ray(Plyr, State, PosX, PosY, _, _) :-                    % Base case: we've found a stone that belongs to Plyr
    in_bounds([PosX,PosY]),
    get(State, [PosX,PosY], Plyr), !.

flip_ray(Plyr, State, PosX, PosY, DirectionX, DirectionY) :-  % Reccursive case: check if stone belongs to opponent and is within bounds
    in_bounds([PosX,PosY]),
    opponent(Plyr, Opp),
    get(State, [PosX,PosY], Opp),
    X2 is PosX + DirectionX,
    Y2 is PosY + DirectionY,
    flip_ray(Plyr, State, X2, Y2, DirectionX, DirectionY).

moves(Plyr, State, MvList) :-                                 % Returns all legal moves
  findall([PosX,PosY], validmove(Plyr, State, [PosX,PosY]), Raw),
  (
    Raw==[] -> MvList=[n];
    MvList=Raw
  ).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Plyr, Pass, State, State, NextPlyr) :-
    (Pass == n ; Pass == null),   
    writeln('We enter Pass-nextState'),        
    opponent(Plyr, NextPlyr).


nextState(Plyr, [X0,Y0], State, NewState, NextPlyr) :- % Real move: Place stone, flip sandwich, switch player
    % format('DEBUG nextState (move): Plyr=~w trying [~w,~w]~n',[Plyr,X0,Y0]),
    set(State, State1, [X0,Y0], Plyr),
    findall(PosList,
            ( direction(DirectionX,DirectionY),
              sandwich(Plyr, State, [X0,Y0], DirectionX, DirectionY),
              X1 is X0 + DirectionX,  Y1 is Y0 + DirectionY,
              collect_flip(Plyr, State, X1, Y1, DirectionX, DirectionY, PosList)
            ),
            FlipLists),
    append(FlipLists, ToFlip),  % We make sure to flatten the list -> [[], [], ...]
    flip_all(Plyr, State1, ToFlip, State2),
    opponent(Plyr, NextPlyr),
    NewState = State2.

collect_flip(Plyr, State, PosX, PosY, _, _, []) :-     % Base case: we've encountered a stone that belongs to Plyr
    get(State, [PosX,PosY], Plyr), !.

collect_flip(Plyr, State, PosX, PosY, DirectionX, DirectionY, [[PosX,PosY]|Rest]) :-    % Reccursive case: We've encountered a stone that belongs to opponent 
    opponent(Plyr, Opp),
    get(State, [PosX,PosY], Opp),
    X2 is PosX + DirectionX,  Y2 is PosY + DirectionY,
    collect_flip(Plyr, State, X2, Y2, DirectionX, DirectionY, Rest).

flip_all(_, State, [], State).  % Base case: nothing to flip

flip_all(Plyr, State, [[PosX,PosY]|Rest], FinalState) :- % Reccursive case: flip all stones in [[PosX,PosY]|Rest]
    set(State, MidState, [PosX,PosY], Plyr),
    flip_all(Plyr, MidState, Rest, FinalState).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State, [PosX,PosY]) :-
    in_bounds([PosX,PosY]),
    get(State, [PosX,PosY], .),              % square must be empty
    direction(DirectionX, DirectionY),
    sandwich(Plyr, State, [PosX,PosY], DirectionX, DirectionY),
    !.    


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

% Base case: so we handle null-moves correctly
h(_,0).

% If player 1 has won, return -100
h(State, -100):- 
	winner(State, 1), !.

% If Player 2 wins, return +100
h(State, 100) :- 
	winner(State, 2), !.

% If we reach a tie, we return 0
h(State, 0) :- 
	tie(State), !.

% Otherwise, count the value to be returned bu stone differance
h(State, Val) :-
	countScore(State, P1, P2),
	Val is P1 - P2.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.


lowerBound(-101).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(101).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column PosX and row PosY
% set(Board, NewBoard, [PosX, PosY], Value):
%    : set Value at column PosX row PosY in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column PosX and row PosY (indexing starts at 0).
% Do not change get:

get( Board, [PosX, PosY], Value) :- 
	nth0( PosY, Board, ListY), 
	nth0( PosX, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [PosX, PosY], Value): set the value of the board at position
% column PosX and row PosY to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [PosX, 0], Value) :-
    setInList(Row, NewRow, PosX, Value). 

set( [Row|RestRows], [Row|NewRestRows], [PosX, PosY], Value) :-
    PosY > 0, 
    Y1 is PosY-1, 
    set( RestRows, NewRestRows, [PosX, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
