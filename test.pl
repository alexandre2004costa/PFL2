board1([
    ['W', 'S', 'S', 'S', 'S', 'S'],
    ['W', 'S', 'S', 'S', 'S', 'S'],
    ['W', 'W', 'S', 'S', 'S', 'S'],
    ['S', 'W', 'S', 'S', 'S', 'S'],
    ['W', 'W', 'S', 'S', 'W', 'W'],
    ['B', 'B', 'S', 'S', 'B', 'B']
]).


levels1([
    [1, 0, 0, 0, 0, 0],
    [1, 0, 0, 0, 0, 0],
    [1, 1, 0, 0, 0, 0],
    [0, 1, 0, 0, 0, 0],
    [1, 1, 0, 0, 1, 1],
    [1, 1, 0, 0, 1, 1]    
]).

board2([
    ['W', 'S', 'S', 'S', 'S'],
    ['W', 'S', 'S', 'S', 'S'],
    ['W', 'W', 'S', 'S', 'S'],
    ['S', 'W', 'S', 'S', 'S'],
    ['B', 'B', 'S', 'S', 'W']
]).


levels2([
    [1, 0, 0, 0, 0],
    [1, 0, 0, 0, 0],
    [1, 1, 0, 0, 0],
    [0, 1, 0, 0, 0],
    [1, 1, 0, 0, 1] 
]).

value2([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Player, Value):-
    value([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Value), !.

choose_move2(GameState, Level, Move):-
    choose_move(GameState, Level, Move, _), !.
    
teste:-
    board2(B),
    levels2(L),
    value2(['p1', B, L, 'p2', 10, red, blue, 1, 1], 'p1', Ratio),
    display_game(['p1', B, L, 'p2', 10, red, blue, 1, 1, Ratio]), % Covers gameOver 
    valid_moves(['p1', B, L, 'p2', 10, red, blue, 1, 1], Moves), write(Moves), nl,  
    choose_move2(['p1', B, L, 'p2', 10, red, blue, 1, 1], 2, [N,X,Y]),
    move(['p1', B, L, 'p2', 10, red, blue, 1, 1], [N,X,Y], [P, B2, L2, OP, M2, C1, C2, BSI, BST]),
    value2([P, B2, L2, OP, M2, C1, C2, BSI, BST], 'p2', NewRatio),
    display_game([P, B2, L2, OP, M2, C1, C2, BSI, BST, NewRatio]). % Covers gameOver   
    