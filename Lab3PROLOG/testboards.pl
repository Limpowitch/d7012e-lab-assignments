%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testboards to use when working on Lab Assignment P3 %%
%% D7012E Declarative languages, LTU                   %%
%% By Håkan Jonsson, 2018                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% 1. Original boards (the ones that came with the assignment)
%%

testBoard1([ [.,.,.,.,.,.], 																
             [.,1,.,.,.,.],
             [.,.,2,1,.,.],
             [.,.,1,2,.,.],
             [.,.,.,.,1,.],
             [.,.,.,.,.,.] ]).

testBoard2([ [.,2,.,.,.,2], 																
             [.,.,1,.,1,.],
             [.,.,.,1,.,.],
             [.,.,1,1,1,.],
             [.,1,.,1,.,.],
             [.,.,.,2,.,.] ]).

testBoard3([ [.,.,.,2,.,.], 																
             [.,2,.,1,1,.],
             [2,1,1,1,.,.],
             [2,1,1,.,1,2],
             [.,1,.,1,.,.],
             [2,.,.,2,2,.] ]).

%%
%% 2. Testing to flip in various directions
%%

% both players can move but when player 1 moves, that move flips from right to left
% along the top row
flipRLtop([[.,1,2,2,2,.],																	%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- flipRLtop(S))), play.																
	   [.,.,.,.,.,.],
	   [.,.,.,.,.,.],
	   [.,.,.,.,.,.],
	   [.,.,.,.,.,.],
	   [.,.,.,.,.,.]]).

% only player 2 can move, and that move flips from left to right							%false. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- flipLRbottom(S))), play.
% along the bottom row																		%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- flipLRbottom(S))), play.
flipLRbottom([[.,.,.,.,.,.],																
	      [.,.,.,.,.,.],
	      [.,.,.,.,.,.],
	      [.,.,.,.,.,.],
	      [.,.,.,.,.,.],
	      [.,1,1,1,1,2]]).

% only player 2 can move, and that move flips from top to bottom
% along the left column
flipTBleft([[.,.,.,.,.,.],																	%false. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- flipTBleft(S))), play.
            [1,.,.,.,.,.],																	%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- flipTBleft(S))), play.
            [1,.,.,.,.,.],
            [1,.,.,.,.,.],
            [1,.,.,.,.,.],
            [2,.,.,.,.,.]]).

% only player 1 can move, and that move flips from bottom to top
% along the right column
flipBTright([[.,.,.,.,.,1],																	%false. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- flipBTright(S))), play.
	     [.,.,.,.,.,2],																		%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- flipBTright(S))), play.
	     [.,.,.,.,.,2],
	     [.,.,.,.,.,2],
	     [.,.,.,.,.,2],
	     [.,.,.,.,.,.]]).

% only player 1 can move, and that move flips along the main
% diagional from upper left to lower right
flipDiagULtoLR([[.,.,.,.,.,.],																%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- flipDiagULtoLR(S))), play.
		[.,2,.,.,.,.],
		[.,.,2,.,.,.],
		[.,.,.,2,.,.],
		[.,.,.,.,1,.],
		[.,.,.,.,.,.]]).

% only player 2 can move, and that move flips along the main
% diagional from upper right to lower left
flipDiagURtoLL([[.,.,.,.,.,.],																%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- flipDiagURtoLL(S))), play.
		[.,.,.,.,1,.],
		[.,.,.,1,.,.],
		[.,.,1,.,.,.],
		[.,1,.,.,.,.],
		[2,.,.,.,.,.]]).

% no moves possible, and no flips
noMovesNoFlipsA([[1,2,1,2,1,2],																%false. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- noMovesNoFlipsA(S))), play.
		 [2,1,1,1,2,2],																		%false. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- noMovesNoFlipsA(S))), play.
		 [1,1,.,1,1,1],
		 [2,1,1,1,2,2],
		 [1,2,1,2,1,2],
		 [2,2,1,2,2,1]]).

% no moves possible, and no flips
noMovesNoFlipsB([[2,1,2,1,2,1],
		 [1,2,2,2,1,1],
		 [2,2,.,2,2,2],
		 [1,2,2,2,1,1],
		 [2,1,2,1,2,1],
		 [1,1,2,1,1,2]]).

% player 1 can move, and that move flips left and right only
flipLRonly1([[.,.,.,2,.,.],																	%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- flipLRonly1(S))), play.
	     [.,.,.,1,.,.],
	     [.,.,.,1,.,.],
	     [1,2,2,.,2,1],
	     [.,.,.,1,.,.],
	     [.,.,.,2,.,.]]).

% only player 1 can move, and that move flips in all 8 directions
flipAll8Dirs1([[1,2,1,2,1,2],																%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- flipAll8Dirs1(S))), play.
	       [2,2,2,2,2,2],
	       [1,2,.,2,2,1],
	       [2,2,2,2,2,2],
	       [1,2,2,2,2,2],
	       [2,2,1,2,2,1]]).

% only player 2 can move, and that move flips in all 8 directions							
flipAll8Dirs2([[2,2,2,2,2,2],																%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- flipAll8Dirs2(S))), play.
	       [2,1,2,1,2,2],
	       [2,2,1,1,1,2],
	       [2,1,1,.,1,2],
	       [2,2,1,1,1,2],
	       [2,2,2,2,2,2]]).


%%
%% 3. Testing that TIE is detected
%%

% both players can make a move; then a tie with a full board
tieInTwoMovesFullBoard([[.,2,2,1,2,2], 														%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- tieInTwoMovesFullBoard(S))), play.
			[2,2,2,2,2,1], 
			[2,2,2,2,2,1],
			[2,2,2,1,1,1], 
			[2,2,2,2,1,1],
			[1,1,1,1,1,.]]).


% an immediate tie with 4 unplayable squares
tieFourEmptyInCorners([[.,2,2,2,2,.],														%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- tieFourEmptyInCorners(S))), play.
		       [1,2,2,2,1,1],
		       [1,2,2,1,1,1],
		       [1,2,1,2,1,1],
		       [1,1,1,1,2,1],
		       [.,2,2,2,2,.]]).


% an immediate tie with 2 unplayable squares
tieFourEmptyOnBorders([[2,2,.,.,1,1],														%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- tieFourEmptyOnBorders(S))), play.
		       [1,1,1,2,2,2],
		       [1,1,1,2,2,2],
		       [1,1,1,2,2,2],
		       [1,1,1,2,2,2],
		       [1,1,.,.,2,2]]).


% player 1 can make 1 move, then tie with four empty
tieFourEmptyOnly1canMove([[.,2,2,2,2,.],													%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- tieFourEmptyOnly1canMove(S))), play.
			  [1,2,2,2,1,1],
			  [1,2,2,1,1,1],
			  [2,2,1,2,1,1],
			  [.,1,1,1,2,1],
			  [.,2,2,2,2,.]]).

% only player 1 can make a move, and then it's a 3-3 tie
tie30emptyOnly1canMove([[.,.,.,.,.,.],														%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,1) :- tie30emptyOnly1canMove(S))), play.
			[.,.,.,.,.,.],
			[2,.,2,1,2,2],
			[.,.,.,.,.,.],
			[.,.,.,.,.,.],
			[.,.,.,.,.,.]]).

% only player 2 can make a move, and then it's a 3-3 tie
tie30emptyOnly2canMove([[.,.,.,.,.,.],														%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- tie30emptyOnly2canMove(S))), play.
			[.,.,.,.,.,.],
			[1,.,1,2,1,1],
			[.,.,.,.,.,.],
			[.,.,.,.,.,.],
			[.,.,.,.,.,.]]).


%%
%% 4. Testing that WINNER is detected
%%

% both players can make one move each, and then player 1 wins
winInTwoMovesFullBoard([[.,2,1,1,1,2], 														%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- winInTwoMovesFullBoard(S))), play.
			[2,2,2,2,2,1], 
			[1,2,2,2,2,1],
			[1,2,2,1,2,1], 
			[1,2,2,2,1,1],
			[2,1,1,1,1,.]]).

% player 1 is an immediate winner, 0-36
onlyTwos([[2,2,2,2,2,2], 																	%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- onlyTwos(S))), play.
	  [2,2,2,2,2,2],
	  [2,2,2,2,2,2], 
	  [2,2,2,2,2,2], 
	  [2,2,2,2,2,2], 
	  [2,2,2,2,2,2] ]).


% player 2 is an immediate winner, 0-36
onlyOnes([[1,1,1,1,1,1], 																	%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- onlyOnes(S))), play.
	  [1,1,1,1,1,1],
	  [1,1,1,1,1,1], 
	  [1,1,1,1,1,1], 
	  [1,1,1,1,1,1], 
	  [1,1,1,1,1,1] ]).


%%
%% 5. Testing null moves
%%

% player 2 has no move, but 1 has two; then 1 wins
forcing2toDoNullMove([[.,.,.,.,.,.],														%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- forcing2toDoNullMove(S))), play.
		      [.,.,.,.,.,2],
		      [.,.,.,.,.,2],
		      [.,.,.,.,.,2],
		      [.,.,.,.,.,2],
		      [.,2,2,2,2,1]]).

% player 1 has no moves, but 2 has two; then 2 wins
forcing1toDoNullMoves([[.,.,.,.,.,.],														%true. dynamic(initialize/2), retractall(initialize(_,_)), assertz((initialize(S,2) :- forcing1toDoNullMoves(S))), play.	
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,1,1,1,1,2]]).
