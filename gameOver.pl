

%Process first row looking for 'W' to start dfs 
process_line(_, _, [], _, _,_,false).

process_line(Board, Y, [PlayerColor|Rest], X, PlayerColor, Visited, Success) :-
    NextX is X+1,
    is_valid_cell(Board, [X,Y], Color), 
    \+ member([X, Y], Visited),
    dfs(Board, [X, Y], Visited, PlayerColor, NewVisited, DfsSuccess),
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
if_valid_dfs(Board, [Col, 0], VisitedIn, 'W', VisitedIn, true).
% Base case for Black win
if_valid_dfs(Board, [11, Row], VisitedIn, 'B', VisitedIn, true).

if_valid_dfs(Board, [Col, Row], VisitedIn, Color, VisitedOut, Success) :-
    is_valid_cell(Board, [Col, Row], Color),
    dfs(Board, [Col, Row], VisitedIn, Color, VisitedOut, Success).

if_valid_dfs(_, _, VisitedIn, _, VisitedIn, false).


dfs(Board, [Col, Row], VisitedIn, Color, VisitedOut, Success) :-
    \+ member([Col, Row], VisitedIn),       
    NewVisited = [[Col, Row] | VisitedIn],   

    NewRow1 is Row - 1,
    if_valid_dfs(Board, [Col, NewRow1], NewVisited, Color, VisitedOutDown, SuccessDown),
    (SuccessDown -> Success = true, ! ;   

    NewRow2 is Row + 1,
    if_valid_dfs(Board, [Col, NewRow2], VisitedOutDown, Color, VisitedOutUp, SuccessUp),
    (SuccessUp -> Success = true, ! ;

    NewCol1 is Col - 1,
    if_valid_dfs(Board, [NewCol1, Row], VisitedOutUp, Color, VisitedOutLeft, SuccessLeft),
    (SuccessLeft -> Success = true, ! ;

    NewCol2 is Col + 1,
    if_valid_dfs(Board, [NewCol2, Row], VisitedOutLeft, Color, VisitedOut, SuccessRight),
    (SuccessRight -> Success = true, ! ;

    Success = false
    )))).

game_over([Player, [FirstLine|Board], Levels, OtherPlayer, 0],  'T').
game_over([Player, [FirstLine|Board], Levels, OtherPlayer, MovesPlayed], Winner) :-
    process_line([FirstLine|Board], 10, FirstLine, 1, 'W',[], WhiteWins),
    process_column([FirstLine|Board], 1, [FirstLine|Board], 1, 'B', [], BlackWins),
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer; WhiteWinner = OtherPlayer, BlackWinner = Player),
    ( WhiteWins, BlackWins -> Winner = 'T'    
    ; WhiteWins -> Winner = WhiteWinner         
    ; BlackWins -> Winner = BlackWinner         
    ; Winner = none                           
    ).