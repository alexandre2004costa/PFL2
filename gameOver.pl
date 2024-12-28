

%Process first row looking for 'W' to start dfs 
process_line(_, _, [], _, _,false).

process_line(Board, Y, [PlayerColor|Rest], X, PlayerColor, Success) :-
    NextX is X+1,
    dfs(Board, [X, Y], [], PlayerColor, DfsSuccess),
    ( DfsSuccess = true -> Success = true ; process_line(Board, Y, Rest, NextX, PlayerColor, Success) ).

process_line(Board, Y, [_|Rest], X, PlayerColor, Success) :-
    NextX is X+1,
    process_line(Board, Y, Rest, NextX, PlayerColor, Success).

%Process first column(first element of each row) looking for 'B' to start dfs 
process_column(_, _, [], _, _,false).

process_column(Board, Y, [[PlayerColor|Line]|Lines], X, PlayerColor, Success):-
    NextY is Y+1,
     dfs(Board, [X, Y], [], PlayerColor, DfsSuccess),
    ( DfsSuccess = true -> Success = true ; process_column(Board, NextY, Lines, X, PlayerColor, Success) ).  

process_column(Board, Y, [[_|Line]|Lines], X, PlayerColor, Success):-
    NextY is Y+1,
    process_column(Board, NextY, Lines, X, PlayerColor, Success).    

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

% Base case for White win
dfs(Board, [Col, 0], Visited, 'W', true).
% Base case for Black win
dfs(Board, [11, Row], Visited, 'B', true).

dfs(Board, [Col, Row], Visited, Color, Success) :-
    is_valid_cell(Board, [Col, Row], Color), % same color, inside boundaries
    \+ member([Col, Row], Visited), % not visited
    append(Visited, [[Col, Row]], NewVisited), % add to visited
    % see close ones
    (   NewRow1 is Row - 1, 
        dfs(Board, [Col, NewRow1], NewVisited, Color, Success)
    ;   NewRow2 is Row + 1, 
        dfs(Board, [Col, NewRow2], NewVisited, Color, Success)
    ;   NewCol1 is Col - 1, 
        dfs(Board, [NewCol1, Row], NewVisited, Color, Success)
    ;   NewCol2 is Col + 1, 
        dfs(Board, [NewCol2, Row], NewVisited, Color, Success)
    ),
    Success = true. % in case of any Success

game_over([Player, [FirstLine|Board], Levels, OtherPlayer, 0],  'T'). % Maybe we need to check for win 1 last time here
game_over([Player, [FirstLine|Board], Levels, OtherPlayer, MovesPlayed], Winner) :-
    process_line([FirstLine|Board], 10, FirstLine, 1, 'W', WhiteWins),
    process_column([FirstLine|Board], 1, [FirstLine|Board], 1, 'B', BlackWins),
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer; WhiteWinner = OtherPlayer, BlackWinner = Player),
    ( WhiteWins, BlackWins -> Winner = 'T'    
    ; WhiteWins -> Winner = WhiteWinner         
    ; BlackWins -> Winner = BlackWinner         
    ; Winner = none                           
    ).
