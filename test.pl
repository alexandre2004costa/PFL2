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


teste:-
    board2(B),
    levels2(L),
    value(['p1', B, L, 'p2', 10, red, blue, 1, 1], 'p1', Ratio),
    display_game(['p1', B, L, 'p2', 10, red, blue, 1, 1, Ratio]), % Covers gameOver 
    valid_moves(['p1', B, L, 'p2', 10, red, blue, 1, 1], Moves), write(Moves), nl,  
    choose_move(['p1', B, L, 'p2', 10, red, blue, 1, 1], 2, [N,X,Y]),
    move(['p1', B, L, 'p2', 10, red, blue, 1, 1], [N,X,Y], [P, B2, L2, OP, M2, C1, C2, BSI, BST]),
    value([P, B2, L2, OP, M2, C1, C2, BSI, BST], 'p2', NewRatio),
    display_game([P, B2, L2, OP, M2, C1, C2, BSI, BST, NewRatio]). % Covers gameOver   
    