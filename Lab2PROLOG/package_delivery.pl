% Move from R1 to R2
move(
    state(r1, holding, Brasskey, Package, TotalItems),
    walk(r1, r2),
    state(r2, holding, Brasskey, Package, TotalItems)
    ).

% Move from R2 to R1
move(
    state(r2, holding, Brasskey, Package, TotalItems),
    walk(r2, r1),
    state(r1, holding, Brasskey, Package, TotalItems)
    ).

% Move from R1 to R3
move(
    state(r1, Steelkey, holding, Package, TotalItems),
    walk(r1, r3),
    state(r3, Steelkey, holding, Package, TotalItems)
    ).

% Move from R3 to R1
move(
    state(r3, Steelkey, holding, Package, TotalItems),
    walk(r3, r1),
    state(r1, Steelkey, holding, Package, TotalItems)
    ).

%Grab Steel Key
grab(
    state(CurrentRoom, CurrentRoom, Brasskey, Package, TotalItems),
    grab(steelkey, CurrentRoom),
    state(CurrentRoom, holding, Brasskey, Package, NewTotalItems)) :-
    TotalItems < 2,
    NewTotalItems is TotalItems + 1.

%Grab Brass Key
grab(
    state(CurrentRoom, Steelkey, CurrentRoom, Package, TotalItems),
    grab(brasskey, CurrentRoom),
    state(CurrentRoom, Steelkey, holding, Package, NewTotalItems)) :-
    TotalItems < 2,
    NewTotalItems is TotalItems + 1.

%Grab Package
grab(
    state(CurrentRoom, Steelkey, Brasskey, CurrentRoom, TotalItems),
    grab(package, CurrentRoom),
    state(CurrentRoom, Steelkey, Brasskey, holding, NewTotalItems)) :-
    TotalItems < 2,
    NewTotalItems is TotalItems + 1.

%Drop Steel Key
drop(
    state(CurrentRoom, holding, Brasskey, Package, TotalItems),
    drop(steelkey, CurrentRoom),
    state(CurrentRoom, CurrentRoom, Brasskey, Package, NewTotalItems)) :-
    NewTotalItems is TotalItems - 1.

%Drop Brass Key
drop(
    state(CurrentRoom, Steelkey, holding, Package, TotalItems),
    drop(brasskey, CurrentRoom),
    state(CurrentRoom, Steelkey, CurrentRoom, Package, NewTotalItems)) :-
    NewTotalItems is TotalItems - 1.

%Drop Package
drop(
    state(CurrentRoom, Steelkey, Brasskey, holding, TotalItems),
    drop(package, CurrentRoom),
    state(CurrentRoom, Steelkey, Brasskey, CurrentRoom, NewTotalItems)) :-
    NewTotalItems is TotalItems - 1.

solveR(Start, MaxDepth, Moves) :-
    solveR(Start, MaxDepth, [Start], Moves),
    !,
    write_term(Moves, [max_depth(0)]),          % Had to add this to ensure we get the full printout
    nl.

solveR(state(_,_,_,r2,_), _, _, []).            % Our final state we try to reach "stop when package = r2"

solveR(State, N, Visited, [Action|Rest]) :-     % For each N, we check if move, grab, drop is legal, and if so, execute it
    N > 0,
    (   move(State,   Action, Next)
    ;   grab(State,   Action, Next)
    ;   drop(State,   Action, Next)
    ),
    \+ member(Next, Visited),                   % Ensures we dont revisit previous state, fixes infinate loops
    N1 is N - 1,
    solveR(Next, N1, [Next|Visited], Rest).     % Recursively call itself for next action (N1)

% invoke with: solveR(state(r1,r1,r2,r3,0), 15, _)
    
