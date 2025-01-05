
% is_valid_cell(+Board, +[Col, Row], +Color)
% Checks if the specified cell ([Col, Row]) is valid for a piece of the given color 
is_valid_cell(Board, [Col, Row], 'W') :-
    length(Board, Size),
    Col2 is Col - 1, % Transformation to 0 index left-top coordenates 
    Row2 is Size - Row,
    Row2 >= 0,
    Col2 =< Size,
    Col2 >= 0,
    valid_white_cell(Board, Row2, Col2, Size).

is_valid_cell(Board, [Col, Row], 'B') :-
    length(Board, Size),
    Col2 is Col - 1, % Transformation to 0 index left-top coordenates 
    Row2 is Row - 1,
    Row2 >= 0,
    Row2 =< Size,
    Col2 >= 0,
    valid_black_cell(Board, Row2, Col2, Size).

% valid_white_cell(+Board, +Row, +Col, +Size)
% Checks if a 'W' piece is valid for the given row and column

% For the bottom row, checks if a 'W' piece can win by being in that row
valid_white_cell(Board, Row2, Col2, Row2):-
    Size1 is Row2 - 1,
    get_value(Board, Size1, Col2, 'W').

% Otherwise, simply verifies if the cell contains 'W'
valid_white_cell(Board, Row2, Col2, _):-
    get_value(Board, Row2, Col2, 'W').

% valid_black_cell(+Board, +Row, +Col, +Size)
% Checks if a 'B' piece is valid for the given row and column

% For the rightmost column, checks if a 'B' piece can win by being in the right-most column
valid_black_cell(Board, Row2, Col2, Col2):-
    Size1 is Col2 - 1,
    get_value(Board, Row2, Size1, 'B').
    
% Otherwise, simply verifies if the cell contains 'B'
valid_black_cell(Board, Row2, Col2, _):-
     get_value(Board, Row2, Col2, 'B').

% process_line(+Board, +Size, +Y, +Line, +X, +Stack, -FinalStack)
% Processes a line of the board to find all 'W' pieces
% Stops when all cells in the row have been processed
% Adds 'W' pieces to the stack and skips others

process_line(_, Size, _, _, Size1, Stack, Stack) :- Size1 is Size + 1, !. % Stops when X = 11, out of board
process_line(_, Size, _, _, Size1, Stack, Stack) :- Size1 is Size + 1, !. % Stops when X = 9, out of board

% Adds a 'W' piece found in the row to the stack and continues processing
process_line(Board, Size, Y, ['W' | Line], X, Stack, FinalStack) :-
    TempStack = [[X, Y] | Stack],
    X1 is X + 1,
    process_line(Board, Size, Y, Line, X1, TempStack, FinalStack), !.

% Skips cells that are not 'W' and continues processing the row.
process_line(Board, Size, Y, [_ | Line], X, Stack, FinalStack) :-
    X1 is X + 1,
    process_line(Board, Size, Y, Line, X1, Stack, FinalStack), !.


% process_column(+Board, +BoardSize, +Y, +Lines, +X, +Stack, -FinalStack)
% Processes a column of the board to find all 'B' pieces
% Stops when all rows in the column have been processed
% Adds 'B' pieces to the stack and skips others

process_column(_, Size, Size1, _, _, Stack, Stack) :- Size1 is Size + 1, !.
process_column(_, Size, Size1, _, _, Stack, Stack) :- Size1 is Size + 1, !.

% Adds a 'B' piece found in the column to the stack and continues processing
process_column(Board, Size, Y, [['B' | _] | Lines], X, Stack, FinalStack) :-
    TempStack = [[X, Y] | Stack],
    Y1 is Y + 1,
    process_column(Board, Size, Y1, Lines, X, TempStack, FinalStack), !.

% Skips cells that are not 'B' and continues processing the column.
process_column(Board, Size, Y, [[_ | _] | Lines], X, Stack, FinalStack) :-
    Y1 is Y + 1,
    process_column(Board, Size, Y1, Lines, X, Stack, FinalStack), !.


% dfs(+Board, +Color, +Stack, +Visited, -LastVisited)
% Performs a Depth-First Search (DFS) starting with the given stack 
% Marks visited cells and returns the final list of visited cells 

dfs(_, _, [], Visited, Visited) :- !. % Empty stack, base case

dfs(Board, Color, [[X, Y] | Stack], Visited, LastVisited) :-
    \+ member([X, Y], Visited), % If is not visited
    NewVisited = [[X, Y] | Visited], % Add to visited 
    add_valid_neighbors(Board, Color, [X, Y], Stack, NewStack), % Add to stack neighbors
    dfs(Board, Color, NewStack, NewVisited, LastVisited).

dfs(Board, Color, [_ | Stack], Visited, LastVisited) :- % Ignore visited cells
    dfs(Board, Color, Stack, Visited, LastVisited).

% Add up, down, left and right neighbors if they are in a valid cell
add_valid_neighbors(Board, Color, [X, Y], Stack, NewStack) :-
    Y1 is Y - 1,
    add_if_valid(Board, Color, [X, Y1], Stack, TempStack1),
    Y2 is Y + 1,
    add_if_valid(Board, Color, [X, Y2], TempStack1, TempStack2),
    X1 is X - 1,
    add_if_valid(Board, Color, [X1, Y], TempStack2, TempStack3),
    X2 is X + 1,
    add_if_valid(Board, Color, [X2, Y], TempStack3, NewStack).

add_if_valid(Board, Color, Cell, Stack, [Cell | Stack]) :- % Adds to the stack if valid
    is_valid_cell(Board, Cell, Color), !.
add_if_valid(_, _, _, Stack, Stack). % If not valid, leaves the stack unchanged



% verify_white_win(+Visited, -Success)
% Checks if any cell in the visited list belongs to the bottom row (indicating a white win)
verify_white_win([], false).
verify_white_win([[_, 0] | _], true).
verify_white_win([[_, _] | Visited], Success) :-
    verify_white_win(Visited, Success).


% verify_black_win(+Size, +Visited, -Success)
% Checks if any cell in the visited list belongs to the rightmost column (indicating a black win)
verify_black_win(_, [], false).
verify_black_win(Size, [[Size1, _] | _], true):- Size1 is Size + 1.
verify_black_win(Size, [[_, _] | Visited], Success) :-
    verify_black_win(Size, Visited, Success).


% game_over(+GameState, -Winner)
% Determines the winner of the game based on the game state
% The game_over function employs a strategy based on connectivity to determine the winner. 
% It begins by identifying all potential starting points for each player: for White ('W'), these are the cells in the top row, while for Black ('B'), they are the cells in the leftmost column. 
% Using Depth-First Search (DFS), it explores all reachable cells for each player, tracking connected components.
% The strategy focuses on verifying if White's connected cells reach the bottom row, or if Black's reach the rightmost column, as these conditions define their respective victories. 
% To optimize performance, the function avoids unnecessary checks by only expanding from valid starting points and skips revisiting cells. If both conditions are met simultaneously, the game ends in a tie. 
% Returns 'T' if the game ends in a tie (no moves left or white and black win at the same time), 'p1' if white wins, 'p2' if black wins or 'none' otherwise
game_over([_, [_ | _], _, _, 0, _, _, _, _], 'T') :- !. % No left moves, tie.

game_over([_, [FirstLine | Board],  _, _, _, _, _, _, _], Winner) :- % Checks for a win condition for both players
    length([FirstLine | Board], Size),
    process_line([FirstLine | Board], Size, Size, FirstLine, 1, [], Stack1), % Gets all 'W' cells in first row
    dfs([FirstLine | Board], 'W', Stack1, [], Visited), !, % With the 'W' cells in first row, finds all reachable cells
    verify_white_win(Visited, WhiteWin), !, % Verifies if any of the cells is a bottom row cell, that indicates a 'W' win

    process_column([FirstLine | Board], Size, 1, [FirstLine | Board], 1, [], Stack2), % Gets all 'B' cells in first column
    dfs([FirstLine | Board], 'B', Stack2, [], Visited2), !, % With the 'B' cells in first column, finds all reachable cells
    verify_black_win(Size, Visited2, BlackWin), !, % Verifies if any of the cells is a right-most column cell, that indicates a 'B' win

    decide_winner(WhiteWin, BlackWin, Winner). % Decides the winner 

% decide_winner(+WhiteWin, +BlackWin, -Winner) 
% Given white and black wins/losts, return the official winner, if there is one
decide_winner(true, true, 'T').
decide_winner(true, false, 'p1').
decide_winner(false, true, 'p2').
