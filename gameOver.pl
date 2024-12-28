

%Process first row looking for 'W' to start dfs 
process_line(_, _, [], _, _,_,false).

process_line(Board, Y, [PlayerColor|Rest], X, PlayerColor, Visited, Success) :-
    NextX is X+1,
    is_valid_cell(Board, [X,Y], Color), % Verifica célula antes de chamar dfs
    \+ member([X, Y], Visited),
    write([X,Y]), write(Visited), write('Bef'),
    dfs(Board, [X, Y], Visited, PlayerColor, NewVisited, DfsSuccess),
    write(NewVisited), write('Aft'),nl,
    ( DfsSuccess = true -> Success = true ; process_line(Board, Y, Rest, NextX, PlayerColor, NewVisited, Success) ).

process_line(Board, Y, [_|Rest], X, PlayerColor, Visited, Success) :-
    NextX is X+1,
    process_line(Board, Y, Rest, NextX, PlayerColor, Visited, Success).

%Process first column(first element of each row) looking for 'B' to start dfs 
process_column(_, _, [], _, _,_,false).

process_column(Board, Y, [[PlayerColor|Line]|Lines], X, PlayerColor, Visited, Success):-
    NextY is Y+1,
    is_valid_cell(Board, [X,Y], Color), % Verifica célula antes de chamar dfs
    \+ member([X, Y], Visited),
    dfs(Board, [X, Y], Visited, PlayerColor, NewVisited, DfsSuccess),
    ( DfsSuccess = true -> Success = true ; process_column(Board, NextY, Lines, X, PlayerColor, NewVisited, Success) ).  

process_column(Board, Y, [[_|Line]|Lines], X, PlayerColor, Visited, Success):-
    NextY is Y+1,
    process_column(Board, NextY, Lines, X, PlayerColor, Visited, Success).    

is_valid_cell(Board, [Col, Row], 'W') :-
    Col2 is Col-1,
    Row2 is 10-Row,
    Row2 >= 0,
    Col2 =< 20,
    Col2 >= 0,
    get_value(Board, Row2, Col2, 'W').

is_valid_cell(Board, [Col, Row], 'B') :-
    Col2 is Col-1,
    Row2 is Row-1,
    Row2 >= 0,
    Row2 =< 10,
    Col2 >= 0,
    get_value(Board, Row2, Col2, 'B').

% Base case for White win
if_valid_dfs(Board, [Col, 0], VisitedIn, 'W', VisitedIn, true):-write('WIN').
% Base case for Black win
if_valid_dfs(Board, [21, Row], VisitedIn, 'B', VisitedIn, true):-write('WIN').

if_valid_dfs(Board, [Col, Row], VisitedIn, Color, VisitedOut, Success) :-
    is_valid_cell(Board, [Col, Row], Color),
    dfs(Board, [Col, Row], VisitedIn, Color, VisitedOut, Success).

if_valid_dfs(_, _, VisitedIn, _, VisitedIn, false).

% dfs principal
dfs(Board, [Col, Row], VisitedIn, Color, VisitedOut, Success) :-
    \+ member([Col, Row], VisitedIn),       % Garante que a célula não foi visitada antes
    NewVisited = [[Col, Row] | VisitedIn], % Atualiza a lista de visitados
    write('#'), write(NewVisited), write('#'),
    % Tenta explorar direções uma por vez  
    (   
        write('Going down'), nl,
        NewRow1 is Row - 1,
        if_valid_dfs(Board, [Col, NewRow1], NewVisited, Color, VisitedOut, Success),
        Success = true
    ;
        write('Going up'), nl,
        NewRow2 is Row + 1,
        if_valid_dfs(Board, [Col, NewRow2], NewVisited, Color, VisitedOut, Success),
        Success = true
    ;
        write('Going left'), nl,
        NewCol1 is Col - 1,
        if_valid_dfs(Board, [NewCol1, Row], NewVisited, Color, VisitedOut, Success),
        Success = true
    ;
        write('Going right'), nl,
        NewCol2 is Col + 1,
        if_valid_dfs(Board, [NewCol2, Row], NewVisited, Color, VisitedOut, Success),
        Success = true
    ;
        % Se todas as direções falharem, define como falha
        write('All directions failed for '), write([Col, Row]), nl,
        VisitedOut = NewVisited,
        Success = false
    ).


        



game_over([Player, [FirstLine|Board], Levels, OtherPlayer, 24],  'T'). % Maybe we need to check for win 1 last time here
game_over([Player, [FirstLine|Board], Levels, OtherPlayer, MovesPlayed], Winner) :-
    process_line([FirstLine|Board], 10, FirstLine, 1, 'W',[], WhiteWins),
    process_column([FirstLine|Board], 1, [FirstLine|Board], 1, 'B', [], BlackWins),
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer; WhiteWinner = OtherPlayer, BlackWinner = Player),
    ( WhiteWins, BlackWins -> Winner = 'T'    
    ; WhiteWins -> Winner = WhiteWinner         
    ; BlackWins -> Winner = BlackWinner         
    ; Winner = none                           
    ).