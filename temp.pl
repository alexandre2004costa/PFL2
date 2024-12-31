
what_move('PlayerVsPlayer', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit).

what_move('PlayerVsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit).

what_move('PlayerVsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y]), Exit = false.

what_move('Pc_1VsPlayer', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit).

what_move('Pc_1VsPlayer', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y]), Exit = false.

what_move('PlayerVsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit).

what_move('PlayerVsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move(['p2', Board, Levels, 'p1', MovesLeft], 2, [N, X, Y]), Exit = false.

what_move('Pc_2VsPlayer', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit).

what_move('Pc_2VsPlayer', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y]), Exit = false.

what_move('Pc_1VsPc_1', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y]), Exit = false.

what_move('Pc_1VsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y]), Exit = false.

what_move('Pc_1VsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y]), Exit = false.

what_move('Pc_2VsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y]), Exit = false.

what_move('Pc_2VsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y]), Exit = false.

what_move('Pc_2VsPc_2', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, Exit):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y]), Exit = false.
>>>>>>> 354243c0a6dbe4091dd751d1a72fc9e4cc241677