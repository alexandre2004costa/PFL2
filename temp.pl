
is_valid_cell(Board, [Col, Row], 'W') :-
    Col2 is Col-1,
    Row2 is 10-Row,
    Row2 >= 0,
    Col2 =< 10,
    Col2 >= 0,
    get_value(Board, Row2, Col2, 'W').

is_valid_cell(Board, [Col, Row], 'B') :-
    Col2 is Col-1,
    Row2 is Row-1,
    Row2 >= 0,
    Row2 =< 10,
    Col2 >= 0,
    get_value(Board, Row2, Col2, 'B').

% Processa a primeira linha procurando por 'W'
process_line(_, _, _, 11, _, Stack, Stack) :- write('End of line'), !.

process_line(Board, Y, ['W' | Line], X, 'W', Stack, FinalStack) :-
    TempStack = [[X, Y] | Stack],
    X1 is X + 1,
    process_line(Board, Y, Line, X1, 'W', TempStack, FinalStack), !.

process_line(Board, Y, [_ | Line], X, 'W', Stack, FinalStack) :-
    X1 is X + 1,
    process_line(Board, Y, Line, X1, 'W', Stack, FinalStack), !.

% Processa a primeira coluna procurando por 'B'
process_column(_, 11, _, _, _, Stack, Stack) :- write('End of column'), !.

process_column(Board, Y, [['B' | Line] | Lines], X, 'B', Stack, FinalStack) :-
    TempStack = [[X, Y] | Stack],
    Y1 is Y + 1,
    process_column(Board, Y1, Lines, X, 'B', TempStack, FinalStack), !.

process_column(Board, Y, [_ | Lines], X, 'B', Stack, FinalStack) :-
    Y1 is Y + 1,
    process_column(Board, Y1, Lines, X, 'B', Stack, FinalStack), !.

% Verifica se o jogo acabou
game_over([Player, [FirstLine | Board], Levels, OtherPlayer, 0], 'T') :- % Empate
    write('Tie'), !.

game_over([Player, [FirstLine | Board], Levels, OtherPlayer, MovesPlayed], none) :-
    write('Game Over'), nl,
    process_line([FirstLine | Board], 10, FirstLine, 1, 'W', [], Stack1),
    write('Stack 1 :'), write(Stack1), nl, !,  % Impede novas buscas após a primeira solução
    process_column([FirstLine | Board], 1, [FirstLine | Board], 1, 'B', [], Stack2),
    write('Stack 2 :'), write(Stack2), nl, !.



testeY :-
    B = [['S','W','S','S','S','S','W','S','S','S'],
         ['S','W','W','S','S','S','S','S','S','S'],
         ['B','B','B','S','S','S','S','S','S','S'],
         ['S','S','S','S','S','S','S','S','S','S'],
         ['S','S','S','S','W','B','S','S','S','S'],
         ['S','S','S','S','B','W','S','S','S','S'],
         ['S','S','S','S','S','S','W','W','W','W'],
         ['S','S','S','S','S','S','B','B','B','B'],
         ['S','S','S','S','S','S','S','S','S','S'],
         ['S','S','S','S','S','S','S','S','S','S']],
    L = [[0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,1,1,0,0,0,0],
         [0,0,0,0,1,1,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,1],
         [0,0,0,0,0,0,1,1,1,1],
         [0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0]],
    game_over(['p1', B, L, 'p2', 52], Success),
    write(Success), nl,
    write('End').
