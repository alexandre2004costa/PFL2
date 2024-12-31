
is_valid_cell(Board, [Col, Row], 'W') :-
    Col2 is Col-1,
    Row2 is 10-Row,
    Row2 >= 0,
    Col2 =< 10,
    Col2 >= 0,
    ((Row2 = 10, get_value(Board, 9, Col2, 'W'));get_value(Board, Row2, Col2, 'W')).


is_valid_cell(Board, [Col, Row], 'B') :-
    Col2 is Col-1,
    Row2 is Row-1,
    Row2 >= 0,
    Row2 =< 10,
    Col2 >= 0,
    ((Col2 = 10, get_value(Board, Row2, 9, 'B'));get_value(Board, Row2, Col2, 'B')).

% Processa a primeira linha procurando por 'W'
process_line(_, _, _, 11, _, Stack, Stack) :- %write('End of line'), 
    !.

process_line(Board, Y, ['W' | Line], X, 'W', Stack, FinalStack) :-
    TempStack = [[X, Y] | Stack],
    X1 is X + 1,
    process_line(Board, Y, Line, X1, 'W', TempStack, FinalStack), !.

process_line(Board, Y, [_ | Line], X, 'W', Stack, FinalStack) :-
    X1 is X + 1,
    process_line(Board, Y, Line, X1, 'W', Stack, FinalStack), !.

% Processa a primeira coluna procurando por 'B'
process_column(_, 11, _, _, _, Stack, Stack) :- %write('End of column'), 
    !.

process_column(Board, Y, [['B' | Line] | Lines], X, 'B', Stack, FinalStack) :-
    TempStack = [[X, Y] | Stack],
    Y1 is Y + 1,
    process_column(Board, Y1, Lines, X, 'B', TempStack, FinalStack), !.

process_column(Board, Y, [[_ | Line] | Lines], X, 'B', Stack, FinalStack) :-
    Y1 is Y + 1,
    process_column(Board, Y1, Lines, X, 'B', Stack, FinalStack), !.


% Caso base: Sucesso alcançado se a pilha está vazia
dfs(_, _, [], Visited, Visited) :- 
    %write('DFS complete'), 
    nl, !.

% Caso recursivo: Explora os vizinhos de uma célula
dfs(Board, Color, [[X, Y] | Stack], Visited, LastVisited) :-
    % Verifica se o nó atual já foi visitado
    \+ member([X, Y], Visited),
    % Marca o nó atual como visitado
    NewVisited = [[X, Y] | Visited],
    % Gera os vizinhos válidos
    NewY1 is Y - 1,
    (is_valid_cell(Board, [X, NewY1], Color) -> NewStack1 = [[X, NewY1] | Stack] ; NewStack1 = Stack),
    NewY2 is Y + 1,
    (is_valid_cell(Board, [X, NewY2], Color) -> NewStack2 = [[X, NewY2] | NewStack1] ; NewStack2 = NewStack1),
    NewX1 is X - 1,
    (is_valid_cell(Board, [NewX1, Y], Color) -> NewStack3 = [[NewX1, Y] | NewStack2] ; NewStack3 = NewStack2),
    NewX2 is X + 1,
    (is_valid_cell(Board, [NewX2, Y], Color) -> NewStack = [[NewX2, Y] | NewStack3] ; NewStack = NewStack3),

    % Exibe o estado atual
    %write('Visiting: '), write([X, Y]), nl,
    %write('New Visited: '), write(NewVisited), nl,
    %write('New Stack: '), write(NewStack), nl,

    % Continua a busca com a nova pilha e os visitados atualizados
    dfs(Board, Color, NewStack, NewVisited, LastVisited).

% Caso recursivo: Se o nó já foi visitado, ignora-o e segue com o restante da pilha
dfs(Board, Color, [_ | Stack], Visited, LastVisited) :-
    dfs(Board, Color, Stack, Visited, LastVisited).

verify_white_win([], false).
verify_white_win([[X,0] | Visited], true).
verify_white_win([[X,Y] | Visited], Success):-
    verify_white_win(Visited, Success).

verify_black_win([], false).
verify_black_win([[11,Y] | Visited], true).
verify_black_win([[X,Y] | Visited], Success):-
    verify_black_win(Visited, Success).


% Verifica se o jogo acabou
game_over([Player, [FirstLine | Board], Levels, OtherPlayer, 0], 'T') :- % Empate
    %write('Tie'), 
    !.

game_over([Player, [FirstLine | Board], Levels, OtherPlayer, MovesPlayed], none) :- % No chance of winning yet
    MovesPlayed > 49, !.

game_over([Player, [FirstLine | Board], Levels, OtherPlayer, MovesPlayed], Result) :-
    %write('Game Over'), nl,
    process_line([FirstLine | Board], 10, FirstLine, 1, 'W', [], Stack1),
    %write('Stack 1 :'), write(Stack1), nl, !,  % Impede novas buscas após a primeira solução
    dfs([FirstLine | Board], 'W', Stack1, [], Visited),!,
    %write('V 1 :'), write(Visited), nl,  % Impede novas buscas após a primeira solução
    verify_white_win(Visited, Success),!,

    process_column([FirstLine | Board], 1, [FirstLine | Board], 1, 'B', [], Stack2),
    %write('Stack 2 :'), write(Stack2), nl, !,  % Impede novas buscas após a primeira solução
    dfs([FirstLine | Board], 'B', Stack2, [], Visited2),!,
    %write('V 2 :'), write(Visited2), nl,  % Impede novas buscas após a primeira solução
    verify_black_win(Visited2, Success2),!,

    (Success, Success2 -> Result = 'T'; Success -> Result = 'p1'; Success2 -> Result = 'p2'; Result = none).




testeY :-
    B = [['S','W','S','S','S','S','W','S','S','S'],
         ['S','W','W','W','S','S','S','S','S','S'],
         ['B','B','B','W','S','S','S','S','S','S'],
         ['S','S','B','B','B','B','B','B','B','S'],
         ['S','S','W','W','W','B','S','S','S','S'],
         ['S','S','S','W','B','W','S','S','S','S'],
         ['S','S','S','W','S','S','W','W','W','W'],
         ['S','S','S','W','S','S','B','B','B','B'],
         ['S','S','S','W','S','S','S','S','S','S'],
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
