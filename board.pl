
board([
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'W', 'W', 'B', 'B', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'B', 'B', 'W', 'W', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S'],
    ['S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S']
]).

piece_from_number(1, piece1).
piece_from_number(2, piece2).
piece_from_number(3, piece3).
piece_from_number(4, piece4).

piece_coordinates(piece1, [['W','W','W','W'], ['B','B','B','B']]).
piece_coordinates(piece2, [['W','W','B','B'], ['W','W','B','B']]).
piece_coordinates(piece3, [['B','B','B','B'], ['W','W','W','W']]).
piece_coordinates(piece4, [['B','B','W','W'], ['B','B','W','W']]).

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

update_piece([Row|Rest], 1, Col, Piece, [NewRow|Rest]):- %Row of alteration
    update_piece_col(Row, Col, Piece, NewRow).

update_piece([Row|Rest], RowIndex, Col, Piece, [Row|NewRest]):- %Row of alteration
    RowIndex > 1,
    Row1 is RowIndex-1,
    update_piece(Rest, Row1, Col, Piece, NewRest).

update_piece_col([_|Rest], 1, Piece, [Piece|Rest]). %Column of alteration

update_piece_col([Char|Rest], Col, Piece, [Char|Result]):- %Column of alteration
    Col > 1,
    Col1 is Col-1,
    update_piece_col(Rest, Col1, Piece, Result).