
board([
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'W', 'B', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'B', 'W', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S']
]).

levels([
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 1, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 1, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
]).

piece_from_number(1, piece1).
piece_from_number(2, piece2).
piece_from_number(3, piece3).
piece_from_number(4, piece4).

piece_coordinates(piece1, [['W','W'], ['B','B']]).
piece_coordinates(piece2, [['W','B'], ['W','B']]).
piece_coordinates(piece3, [['B','B'], ['W','W']]).
piece_coordinates(piece4, [['B','W'], ['B','W']]).


get_value([Row|_], 0, X, Value):- 
    get_value_in_row(Row, X, Value).

get_value([_|Rest], Y, X, Value):- 
    Y > 0,
    Y1 is Y - 1,
    get_value(Rest, Y1, X, Value).

get_value_in_row([Value|_], 0, Value).
get_value_in_row([_|Rest], X, Value):- 
    X > 0,
    X1 is X - 1,
    get_value_in_row(Rest, X1, Value).


update_piece([Row|Rest], 1, Col, Piece, [NewRow|Rest]):- % Row to update
    update_piece_col(Row, Col, Piece, NewRow).

update_piece([Row|Rest], RowIndex, Col, Piece, [Row|NewRest]):-
    RowIndex > 1,
    Row1 is RowIndex-1,
    update_piece(Rest, Row1, Col, Piece, NewRest).

update_piece_col([_|Rest], 1, Piece, [Piece|Rest]). % Column to update
update_piece_col([Char|Rest], Col, Piece, [Char|Result]):-
    Col > 1,
    Col1 is Col-1,
    update_piece_col(Rest, Col1, Piece, Result).