
% is_valid_cell(+Board, +BoardSize, +[Col, Row], +Color)
% Checks if the specified cell ([Col, Row]) is valid for a piece of the given color 
is_valid_cell(Board, NBoard, [Col, Row], 'W') :-
    Col2 is Col - 1, % Transformation to 0 index left-top coordenates 
    (NBoard = 1 -> Row2 is 10 - Row; NBoard = 2 -> Row2 is 8 - Row),
    Row2 >= 0,
    (NBoard = 1 -> Col2 =< 10; NBoard = 2 -> Col2 =< 8),
    Col2 >= 0,

    (   
        (Row2 = 10, get_value(Board, 9, Col2, 'W')) % This is the bottom row where 'W' wins
    ; 
        get_value(Board, Row2, Col2, 'W')
    ).

is_valid_cell(Board, NBoard, [Col, Row], 'B') :-
    Col2 is Col - 1, % Transformation to 0 index left-top coordenates 
    Row2 is Row - 1,
    Row2 >= 0,
    (NBoard = 1 -> Row2 =< 10; NBoard = 2 -> Row2 =< 8),
    Col2 >= 0,
    (
        (Col2 = 10, get_value(Board, Row2, 9, 'B')) % This is the right column where 'B' wins
    ; 
        get_value(Board, Row2, Col2, 'B')
    ).


% process_line(+Board, +BoardSize, +Y, +Line, +X, +Stack, -FinalStack)
% Used to find 'W' cells in the first row of the board

process_line(_, 1, _, _, 11, Stack, Stack) :- !. % Stops when X = 11, out of board
process_line(_, 2, _, _, 9, Stack, Stack) :- !. % Stops when X = 9, out of board

% Adds a 'W' piece found in the row to the stack and continues processing
process_line(Board, NBoard, Y, ['W' | Line], X, Stack, FinalStack) :-
    TempStack = [[X, Y] | Stack],
    X1 is X + 1,
    process_line(Board, NBoard, Y, Line, X1, TempStack, FinalStack), !.

% Skips cells that are not 'W' and continues processing the row.
process_line(Board, NBoard, Y, [_ | Line], X, Stack, FinalStack) :-
    X1 is X + 1,
    process_line(Board, NBoard, Y, Line, X1, Stack, FinalStack), !.


% process_column(+Board, +BoardSize, +Y, +Lines, +X, +Stack, -FinalStack)
% Used to find 'B' cells in the first column of the board
process_column(_, 1, 11, _, _, Stack, Stack) :- !.
process_column(_, 2, 9, _, _, Stack, Stack) :- !.

% Adds a 'B' piece found in the column to the stack and continues processing
process_column(Board, NBoard, Y, [['B' | _] | Lines], X, Stack, FinalStack) :-
    TempStack = [[X, Y] | Stack],
    Y1 is Y + 1,
    process_column(Board, NBoard, Y1, Lines, X, TempStack, FinalStack), !.

% Skips cells that are not 'B' and continues processing the column.
process_column(Board, NBoard, Y, [[_ | _] | Lines], X, Stack, FinalStack) :-
    Y1 is Y + 1,
    process_column(Board, NBoard, Y1, Lines, X, Stack, FinalStack), !.


% dfs(+Board, +BoardSize, +Color, +Stack, +Visited, -LastVisited)
% Performs a Depth-First Search (DFS) starting with the given stack 
% Marks visited cells (Visited) and returns the final list of visited cells 

dfs(_, _, _, [], Visited, Visited) :- !. % Base case for empty stack, no more cells to find

% Adds a cell to the visited list and explores its neighbors if valid
dfs(Board, NBoard, Color, [[X, Y] | Stack], Visited, LastVisited) :-
    \+ member([X, Y], Visited),
    NewVisited = [[X, Y] | Visited],
    NewY1 is Y - 1,
    (is_valid_cell(Board, NBoard, [X, NewY1], Color) -> NewStack1 = [[X, NewY1] | Stack] ; NewStack1 = Stack),
    NewY2 is Y + 1,
    (is_valid_cell(Board, NBoard, [X, NewY2], Color) -> NewStack2 = [[X, NewY2] | NewStack1] ; NewStack2 = NewStack1),
    NewX1 is X - 1,
    (is_valid_cell(Board, NBoard, [NewX1, Y], Color) -> NewStack3 = [[NewX1, Y] | NewStack2] ; NewStack3 = NewStack2),
    NewX2 is X + 1,
    (is_valid_cell(Board, NBoard, [NewX2, Y], Color) -> NewStack = [[NewX2, Y] | NewStack3] ; NewStack = NewStack3),
    dfs(Board, NBoard, Color, NewStack, NewVisited, LastVisited).

% Skips cells that are already visited and continues DFS
dfs(Board, NBoard, Color, [_ | Stack], Visited, LastVisited) :-
    dfs(Board, NBoard, Color, Stack, Visited, LastVisited).


% verify_white_win(+Visited, -Success)
% Checks if any cell in the visited list belongs to the bottom row (indicating a white win)
verify_white_win([], false).
verify_white_win([[_, 0] | _], true).
verify_white_win([[_, _] | Visited], Success) :-
    verify_white_win(Visited, Success).

% verify_black_win(+BoardSize, +Visited, -Success)
% Checks if any cell in the visited list belongs to the rightmost column (indicating a black win)
verify_black_win(_, [], false).
verify_black_win(1, [[11, _] | _], true).
verify_black_win(2, [[9, _] | _], true).
verify_black_win(NBoard, [[_, _] | Visited], Success) :-
    verify_black_win(NBoard, Visited, Success).


% game_over(+GameState, -Result)
% Determines the result of the game based on the game state and the board size
% Returns 'T' if the game ends in a tie (no moves left or white and black win at the same time), 
%'p1' if white wins, 'p2' if black wins or 'none' otherwise

game_over([_, [_ | _], _, _, 0], _, 'T') :- !. % No left moves, tie.

game_over([_, [_ | _], _, _, MovesPlayed], NBoard, none) :- 
    (NBoard = 1 -> MovesPlayed > 49;    % Need at least 5 moves to win
     NBoard = 2 -> MovesPlayed > 26)     % Need at least 4 moves to win
    , !.

game_over([_, [FirstLine | Board], _, _, _, NBoard], Result) :- % Checks for a win condition for both players
    process_line([FirstLine | Board], NBoard, 10, FirstLine, 1, [], Stack1),
    dfs([FirstLine | Board], NBoard, 'W', Stack1, [], Visited), !,
    verify_white_win(Visited, Success), !,

    process_column([FirstLine | Board], NBoard, 1, [FirstLine | Board], 1, [], Stack2),
    dfs([FirstLine | Board], NBoard, 'B', Stack2, [], Visited2), !,
    verify_black_win(NBoard, Visited2, Success2), !,

    (Success, Success2 -> Result = 'T';
     Success -> Result = 'p1';
     Success2 -> Result = 'p2';
     Result = none).