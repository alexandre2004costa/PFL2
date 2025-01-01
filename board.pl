
% board(-Board)
% Initializes the game board. Each cell is represented by:
% an empty space ('S'), a white part of a block ('W') or a black part of a block ('B').
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

% levels(-BoardLevels)
% Represents the height level of each cell on the board.
% Zero means an empty space and higher numbers represent stacked pieces.
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

% piece_from_number(+Number, -Piece)
% Maps a number to a specific game piece identifier.
piece_from_number(1, piece1).
piece_from_number(2, piece2).
piece_from_number(3, piece3).
piece_from_number(4, piece4).

% piece_coordinates(+Piece, -PieceRepresentation)
% Get the piece representation for the board.
piece_coordinates(piece1, [['W','W'], ['B','B']]).
piece_coordinates(piece2, [['W','B'], ['W','B']]).
piece_coordinates(piece3, [['B','B'], ['W','W']]).
piece_coordinates(piece4, [['B','W'], ['B','W']]).

% get_value(+Board, +Y, +X, -Value)
% Get the value of a cell from the board at coordinates (X, Y).
get_value([Row|_], 0, X, Value):- 
    get_value_in_row(Row, X, Value).
get_value([_|Rest], Y, X, Value):- 
    Y > 0, Y1 is Y - 1,
    get_value(Rest, Y1, X, Value).

% get_value_in_row(+Row, +X, -Value)
% Helper predicate to get a value from a specific row at column X.
get_value_in_row([Value|_], 0, Value).
get_value_in_row([_|Rest], X, Value):- 
    X > 0, X1 is X - 1,
    get_value_in_row(Rest, X1, Value).

% update_piece(+Board, +RowIndex, +Col, +Piece, -NewBoard)
% Updates the board by placing a new piece at the specified row and column.
update_piece([Row|Rest], 1, Col, Piece, [NewRow|Rest]):- % Row to update
    update_piece_col(Row, Col, Piece, NewRow).
update_piece([Row|Rest], RowIndex, Col, Piece, [Row|NewRest]):-
    RowIndex > 1,
    Row1 is RowIndex-1,
    update_piece(Rest, Row1, Col, Piece, NewRest).

% update_piece_col(+Row, +Col, +Piece, -NewRow)
% Helper predicate to update with a new piece a specific column in a row.
update_piece_col([_|Rest], 1, Piece, [Piece|Rest]). % Column to update
update_piece_col([Char|Rest], Col, Piece, [Char|Result]):-
    Col > 1,
    Col1 is Col-1,
    update_piece_col(Rest, Col1, Piece, Result).