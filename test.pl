% This file is only used to test some game states

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

% Tests in smaller boards showing that our functions work for any board size
teste:-
    board2(B),
    levels2(L),
    value(['p1', B, L, 'p2', 10, red, blue, 1, 1], 'p1', Ratio),
    display_game(['p1', B, L, 'p2', 10, red, blue, 1, 1, Ratio]),
    valid_moves(['p1', [['S']], [[0]], 'p2', 10, red, blue, 1, 1], Moves), 
    choose_move(['p1', B, L, 'p2', 10, red, blue, 1, 1], 1, [N,X,Y]),
    move(['p1', B, L, 'p2', 10, red, blue, 1, 1], [N,X,Y], [P, B2, L2, OP, M2, C1, C2, BSI, BST]),
    value([P, B2, L2, OP, M2, C1, C2, BSI, BST], 'p2', NewRatio),
    display_game([P, B2, L2, OP, M2, C1, C2, BSI, BST, NewRatio]),
    game_over([P, B2, L2, OP, M2, C1, C2, BSI, BST], Winner). 
    
% Game states for demonstration

gameOverTie:-
    play_game('Pc_2VsPc_2', red, blue, 2, 2).

gameOverP1:-
    play_game('Pc_2VsPc_1', yellow, cyan, 2, 3).

gameOverP2:- 
    play_game('Pc_1VsPc_2', green, magenta, 1, 2).

gameInput:-
    play_game('PlayerVsPlayer', cyan, cyan, 1, 2).

gameEloJump:-
    play_game('PlayerVsPc_1', green, green, 2, 1).

gameBlock:-
    play_game('PlayerVsPc_2', red, red, 2, 1).

gameAI2win:-
    play_game('Pc_2VsPc_2', blue, blue, 2, 3).