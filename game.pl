:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(gameOver). 
:- consult(board). 
:- consult(display). 
:- consult(colors).
:- consult(menu).
:- consult(test).

% Play -----------------------------------------------------------------------------------------------------------------------------

% play/0
% Opens the game menu and start the game cycle. 
% The cycle starts in the initial menu state, where the user can choose to play, adjust custom configurations for the game or exit.
% By default, the players colors are set to white.
play:- state(initial, white, white, 1, 1).


% play_game(+Mode, +Color1, +Color2, +BoardSize, +BoardStyle)
% Starts the game in the specified mode with given player colors, board size, and board style.
% It initiates the game state and the first turn.
play_game(Mode, Color1, Color2, BoardSize, BoardStyle):-
    initial_state(['p1', 'p2', Color1, Color2, BoardSize, BoardStyle], GameState),
    play_turn(Mode, GameState, 0.5).

% initial_state(+GameConfig, -GameState)
% Sets up the initial configuration of the game, generating the game state.
% It includes the two players, the starting player and the opponent, as well as their respective colors.
% Given the board size and board style, the function gets the corresponding board, levels and moves left.
initial_state([Player, OtherPlayer, Color1, Color2, BoardSize, BoardStyle], [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle]):-
    board(BoardSize, BoardStyle, Board, MovesLeft),
    levels(BoardSize, BoardStyle, Levels).


% play_turn(+Mode, +GameState, +MoveRatio)
% Handles a player turn, displaying the game and a ratio indicating how well each player is. 
% If the game is not over, continues to the next turn.
play_turn(Mode, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], MoveRatio) :-   
    game_over([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Winner),  
    final_ratio(Winner, MoveRatio, FinalRatio),
    display_game([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle, FinalRatio]),
    handle_game(Mode, Winner, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle]).

play_turn(Mode, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], MoveRatio) :-  
    display_game([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle, MoveRatio]),
    what_move(Mode, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, NewMoveRatio, Exit),
    handle_move(Mode, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, NewMoveRatio).


% final_ratio(+Winner, +MoveRatio, -NewMoveRatio)
% Determines a new ratio if there is a winner. 
final_ratio('p1', _, 1).
final_ratio('p2', _, 0).
final_ratio(_, MoveRatio, MoveRatio).

% handle_game(+Mode, +Winner, +GameState)
% Handles the current game state. 
% If there is a tie or a winner, the game ends. If not, it handles the next move.
handle_game(_, 'T', [_, _, _, _, _, Color1, Color2, BoardSize, BoardStyle]) :- 
    print_banner_tie,
    state(winner, Color1, Color2, BoardSize, BoardStyle).
handle_game(_, Winner, [_, _, _, _, _, Color1, Color2, BoardSize, BoardStyle]) :-
    print_banner_final(Winner, Color1, Color2),
    state(winner, Color1, Color2, BoardSize, BoardStyle).


% handle_move(+Mode, +GameState, +Move, +Exit, +NewMoveRatio)
% Processes a player move. 
% If the player does not choose to exit the game, it updates the game state and continues the game.
handle_move(Mode, State, [N, X, Y], false, NewMoveRatio) :-
    move(State, [N, X, Y], NewState),
    play_turn(Mode, NewState, NewMoveRatio).
handle_move(_, [_, _, _, OtherPlayer, _, Color1, Color2, BoardSize, BoardStyle], _, true, _) :-
    print_banner_final(OtherPlayer, Color1, Color2),
    state(winner, Color1, Color2, BoardSize, BoardStyle).


% what_move(+Mode, +GameState, -N, -X, -Y, -MoveRatio, -Exit)
% Gets the moves for different game modes
what_move('PlayerVsPlayer', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(Board, Levels, N, X, Y, Exit),
    player_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('PlayerVsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(Board, Levels, N, X, Y, Exit),
    player_move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('PlayerVsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPlayer', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(Board, Levels, N, X, Y, Exit),
    player_move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('Pc_1VsPlayer', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('PlayerVsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(Board, Levels, N, X, Y, Exit),
    player_move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('PlayerVsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPlayer', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(Board, Levels, N, X, Y, Exit),
    player_move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('Pc_2VsPlayer', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_1', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_2', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move_ratio([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).


% player_move(+GameState, +Move, +Exit, -MoveRatio).
% Get the ratio based on the user player exiting the game or not.
player_move(_, _, true, MoveRatio) :-
    MoveRatio = 0.
player_move(GameState, Move, false, MoveRatio) :-
    move(GameState, Move, NewGameState),
    ratio_value(NewGameState, MoveRatio).


% Validation of Inputs ------------------------------------------------------------------------------------------------------------

% read_option(+Number, -Option) 
% Prompts the user to select an option between 1 and N and stores it. 
read_option(N, Option) :-
    write('Option: '), nl,
    read(Input),
    validate_option(Input, N, Option).

% validate_option(+Input, +Number, -Option)
% Validates the user input for the option
validate_option(Input, N, Option) :-
    integer(Input), Input > 0, Input =< N,
    Option is Input, nl.
validate_option(_, N, Option) :-
    write('Invalid input. '),
    read_option(N, Option).


% read_input(+Board, +Levels, -Number, -X, -Y, -Exit)
% Reads the user input about their move or decision to exit the game.
read_input(Board, Levels, N, X, Y, Exit) :-
    write('Choose the type of block (1-4) or type "exit" to give up: '),
    length(Board, Size),
    read_input_type(Size, Levels, N, X, Y, Exit), nl.

% read_input_type(+Size, +Levels, -Number, -X, -Y, -Exit)
% Reads the type of block input by the user
read_input_type(Size, Levels, N, X, Y, Exit) :-
    read(InputN),
    validate_input_type(Size, Levels, InputN, N, X, Y, Exit).

% validate_input_type(+Size, +Levels, +InputN, -Number, -X, -Y, -Exit) 
% Validates the type of block input by the user or the choice to exit
validate_input_type(Size, Levels, InputN, N, X, Y, false) :-
    integer(InputN), InputN >= 1, InputN =< 4,
    N = InputN,
    read_input_coordinates(Size, Levels, X, Y), nl.
validate_input_type( _, _, 'exit', _, _, _, true).
validate_input_type(Size, Levels, _, N, X, Y, Exit) :-
    write('Invalid. Choose a number between 1 and 4 or type exit.'),
    read_input_type(Size, Levels, N, X, Y, Exit).


% read_input_coordinates(+Size, +Levels, -X, -Y)
% Prompts the user to give X and Y coordinates of the move based on the board.
read_input_coordinates(Size, Levels, X, Y) :-
    Size1 is Size - 1,
    format('Choose the coordinate X (1-~d): ', [Size1]), read(InputX),
    format('Choose the coordinate Y (1-~d): ', [Size1]), read(InputY),
    validate_input_coordinates(Size, Levels, InputX, InputY, X, Y).

% validate_input_coordinates(+Size, +Levels, +InputX, +InputY, -X, -Y)
% Validates the X and Y coordinates entered by the user.
validate_input_coordinates(Size, Levels, InputX, _, X, Y) :-
    \+ integer(InputX),
    write('The X coordinate must be a number'), nl, nl,
    read_input_coordinates(Size, Levels, X, Y).

validate_input_coordinates(Size, Levels, _, InputY, X, Y) :-
     \+ integer(InputY),
    write('The Y coordinate must be a number'), nl, nl,
    read_input_coordinates(Size, Levels, X, Y). 

validate_input_coordinates(Size, Levels, InputX, InputY, X, Y) :-
    Size1 is Size - 1,
    (InputX < 1; InputX > Size1; InputY < 1; InputY > Size1), 
    format('The coordinates must be between 1 and ~d. ', [Size1]), nl, nl,
    read_input_coordinates(Size, Levels, X, Y).  

validate_input_coordinates(Size, Levels, InputX, InputY, X, Y) :-
    validate_coordinates(InputX, InputY, Size, Levels, Valid),
    Valid =:= 1,
    X = InputX, Y = InputY, !.

validate_input_coordinates(Size, Levels, InputX, InputY, X, Y) :-
    validate_coordinates(InputX, InputY, Size, Levels, Valid),
    Valid =:= 0, 
    write('Invalid coordinates. Try again.'), nl,
    read_input_coordinates(Size, Levels, X, Y).


% Game ----------------------------------------------------------------------------------------------------------------------------

% valid_moves(+GameState, -ListOfMoves)
% Generates all valid moves for the current game state.
% The function iterates through all possible combinations of move parameters (N, X, Y), where N represents the piece type, and X and Y represent the coordinates on the board. 
% For each combination, it checks if the coordinates are within the bounds of the board and valid according to the game's rules, using the validate_coordinates predicate.
valid_moves([_, Board, Levels, _, _, _, _, _, _], ListOfMoves) :-
    length(Board, Size),
    setof([N, X, Y], (
        generate_coordinates(1, 4, N),
        generate_coordinates(1, 9, X),
        generate_coordinates(1, 9, Y),
        validate_coordinates(X, Y, Size, Levels, 1)
    ), ListOfMoves).

valid_moves([_, _, _, _, _, _, _, _, _], []).

% generate_coordinates(+Low, +High, -Value)
% Generates all integers between Low and High.
generate_coordinates(Low, High, Low) :-
    Low =< High.
generate_coordinates(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    generate_coordinates(Next, High, Value).

% validate_coordinates(+X, +Y, +Size, +Levels, -Valid)
% Validates if the coordinates (X, Y) can be placed on the board given the levels of the blocks and the board size.
validate_coordinates(X, Y, Size, Levels, Valid) :-
    BoardX is X-1, 
    BoardY is Size - Y,
    BoardX2 is BoardX+1, BoardY2 is BoardY-1,

    1 is X mod 2, 1 is BoardY mod 2, 
    get_value(Levels, BoardY, BoardX, Level_LD), Level_LD is 0,
    get_value(Levels, BoardY, BoardX2, Level_LT), Level_LT is 0,
    get_value(Levels, BoardY2, BoardX, Level_RD), Level_RD is 0,
    get_value(Levels, BoardY2, BoardX2, Level_RT), Level_RT is 0,
    Valid is 1.

validate_coordinates(X, Y, Size, Levels, Valid) :-
    BoardX is X-1, 
    BoardY is Size - Y,
    BoardX2 is BoardX+1, BoardY2 is BoardY-1,

    get_value(Levels, BoardY, BoardX, L1),
    get_value(Levels, BoardY, BoardX2, L2),
    get_value(Levels, BoardY2, BoardX, L3),
    get_value(Levels, BoardY2, BoardX2, L4),
    L2 is L1, L3 is L1, L4 is L1,

    ((0 is L1 mod 2, 1 is X mod 2, 1 is Y mod 2) ; (1 is L1 mod 2, 0 is X mod 2, 0 is Y mod 2)),
    Valid is 1.

validate_coordinates(_, _, _, _, Valid) :-
    Valid is 0.


% move(+GameState, +Move, -NewGameState)
% Responsible for executing a move in the game, updating the game state based on the parameters [N, X, Y], where N represents the type of piece and [X, Y] are the move coordinates. 
% First, the move is validated using the validate_coordinates function to ensure that the position is valid and adheres to the game rules. 
% Then, the piece type (N) is converted into its corresponding configuration (PieceConfig), which defines how it interacts with the board. 
% The function updates the main board by modifying the four cells affected by the 2x2 piece, using the update_piece predicate to apply the changes according to the piece's configuration.
% In addition to the board, the level of the affected cells is updated in the Levels structure, incrementing the value of the same four positions by 1. 
% After all updates are applied, the active player (Player) is swapped with the inactive player (OtherPlayer), and the number of remaining moves (MovesLeft) is decremented by 1.
% Executes a move by placing a block of type N at coordinates (X, Y) on the board, if the move is valid.
move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], [OtherPlayer, Board4, Levels4, Player, Moves1, Color1, Color2, BoardSize, BoardStyle]):-
    length(Board, Size),
    Moves1 is MovesLeft-1,
    NewX is X, 
    NewY is Size - Y, 

    validate_coordinates(X, Y, Size, Levels, 1),  % Check if its valid

    % Get block
    piece_from_number(N, Piece),
    piece_coordinates(Piece, PieceConfig), 
    get_value(PieceConfig, 0, 0, V0),
    get_value(PieceConfig, 0, 1, V1),
    get_value(PieceConfig, 1, 0, V2),
    get_value(PieceConfig, 1, 1, V3),

    % Update board
    update_piece(Board, NewY, NewX, V0, Board1),
    update_piece(Board1, NewY, NewX+1, V1, Board2),
    update_piece(Board2, NewY+1, NewX, V2, Board3),
    update_piece(Board3, NewY+1, NewX+1, V3, Board4),

    % Update levels
    get_value(Levels, NewY, NewX, Level), NewLevel is Level+1,
    update_piece(Levels, NewY, NewX, NewLevel, Levels1),
    update_piece(Levels1, NewY, NewX+1, NewLevel, Levels2),
    update_piece(Levels2, NewY+1, NewX, NewLevel, Levels3),
    update_piece(Levels3, NewY+1, NewX+1, NewLevel, Levels4).



% value(+GameState, +Player, -Value)
% The value function evaluates the game state for a given player, determining how favorable it is. 
% It assigns a neutral value (0.5) if the turn is over. Otherwise, it uses ratio_value to calculate a score (0 to 1) based on potential winning or blocking moves and strategic factors.
% The ratio_value prioritizes winning moves and considers the distribution of favorable blocks on the board. 
% The get_compensation adjusts the score to account for immediate threats or advantages, while get_ratio_value combines block counts and compensation to calculate the final score, clamped to [0, 1].
% This approach balances immediate tactics with broader strategy, providing a fair evaluation of the game state.
% Returns a value indicating how bad or good the game state is to the given player.
value([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Player, Value):-
    ratio_value([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Value), !.

% ratio_value(+GameState, -Value)
% Computes the normalized value (0 to 1), adjusting it based on winning moves and compensation. 
ratio_value([_, _, _, _, 0, _, _, _, _], 0.5).
ratio_value([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], ClampedValue):-
    valid_moves([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, [WMove, _], [BMove, _]),!,
    get_compension(WMove, BMove, Player, Compensation),
    get_ratio_value(WMove, BMove, Player, Board, Compensation, Value),
    ClampedValue is max(0, min(1, Value)).


% get_compension(+WMove, +BMove, +Player, -Compensation)
% Determines the compensation based on winning or blocking moves for the other player.
get_compension(WMove, _, Player, Compensation):-
    WMove \= none, Player = 'p2',
    Compensation = 0.15.
get_compension(_, BMove, Player, Compensation):-
    BMove \= none, Player = 'p1',
    Compensation = -0.15.
get_compension(_, _, _ ,Compensation):-
    Compensation = 0.

% get_ratio_value(+WMove, +BMove, +Player, +Board, +Compensation, -Value)
% Calculates the ratio value of the game state, based on winning and 
% blocking moves, counts of block types and compensation.
get_ratio_value(WMove, _, Player, _, _, Value):-
    WMove \= none, Player = 'p1',
    Value = 1.
get_ratio_value(_, BMove, Player, _, _, Value):-
    BMove \= none, Player = 'p2',
    Value = 0.
get_ratio_value(_, _, Player, Board, Compensation, Value):-
    NumRC is 1, get_block(Player, Block, OtherBlock),
    max_count_col(Board, NumRC, Block, CountColGood, _),
    max_count_row(Board, NumRC, OtherBlock, CountRowBad, _),

    TotalCount is CountColGood + CountRowBad,
    BaseValue is CountColGood / TotalCount,

    Value is BaseValue + Compensation.


% get_block(+Player, -Block, -OtherBlock)
% Maps the player to their corresponding block type and the opponent.
get_block('p1', 'W', 'B').
get_block('p2', 'B', 'W').


% max_count(+Rows, +NumRow, +Block, +CurrentMax, +CurrentRow, -Max, -MaxRow)
% Finds the maximum count of a specific block type.
max_count([], _, _, CurrentMax, CurrentRow, CurrentMax, CurrentRow).
max_count([Row | Rest], NumRow, Block, CurrentMax, CurrentRow, Max, MaxRow):-
    count_block(Row, Block, CountRow),
    update_max(CountRow, CurrentMax, NumRow, CurrentRow, NewMax, NewMaxRow),
    NextRow is NumRow + 1,
    max_count(Rest, NextRow, Block, NewMax, NewMaxRow, Max, MaxRow).

% count_block(+Row, +Block, -Count)
% Counts the occurrences of a specific block type in a given row.
count_block([], _, 0).
count_block([Elem | Rest], Block, Count):-
    count_block(Rest, Block, RestCount),
    Elem = Block, Count is RestCount + 1.
count_block([_ | Rest], Block, Count):-
    count_block(Rest, Block, RestCount),
    Count is RestCount.

% update_max(+CountRow, +CurrentMax, +NumRow, +CurrentRow, -NewMax, -NewMaxRow)
% Compares the current row count with the maximum count so far and updates the maximum if needed.
update_max(CountRow, CurrentMax, NumRow, _, NewMax, NewMaxRow) :-
    CountRow > CurrentMax,
    NewMax = CountRow, NewMaxRow = NumRow.
update_max(CountRow, CurrentMax, _, CurrentRow, NewMax, NewMaxRow) :-
    CountRow =< CurrentMax,
    NewMax = CurrentMax, NewMaxRow = CurrentRow.

% max_count_row(+Board, +NumRC, +Block, -CountRow, -NumRow)
% Finds the row with the maximum count of a specific block type on the board.
max_count_row(Board, NumRC, Block, CountRow, NumRow):-
    max_count(Board, NumRC, Block, -1, -1, CountRow, NumRow).

% max_count_col(+Board, +NumRC, +Block, -CountCol, -NumCol)
% Finds the column with the maximum count of a specific block type on the board.
max_count_col(Board, NumRC, Block, CountCol, NumCol):-
    transpose(Board, TransposedBoard),
    max_count(TransposedBoard, NumRC, Block, -1, -1, CountCol, NumCol).



% is_any_winning_move(+GameState, +Moves, -WhiteWinningMove, -BlackWinningMove)
% Checks if any move in the list of Moves is a winning move for either player, and if there is saves it.
is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [Move], [WMove, WWin], [BMove, BWin]):-
    winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, Winner),
    update_winning_move(Winner, Move, WMove, WWin, BMove, BWin).
   
is_any_winning_move([_, _, _, _, _, _, _, _, _], [_], [none, false], [none, false]).
   
is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, _, BoardStyle], [Move|Moves], [WMove, WWin], [BMove, BWin]):-
    winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, Winner),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, [NextWMove, NextWWin], [NextBMove, NextBWin]),
    check_winner_1(Winner, Move, NextWMove, NextWWin, WMove, WWin),
    check_winner_2(Winner, Move, NextBMove, NextBWin, BMove, BWin).

is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [_|Moves], [NextWMove, NextWWin], [NextBMove, NextBWin]):-
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, [NextWMove, NextWWin], [NextBMove, NextBWin]).

% winning_move(+GameState, +Move, -Winner)
% Checks if the move on the game state results in a win for any player.
winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, Winner):-
    move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, OtherGameState),
    game_over(OtherGameState, Winner).

% update_winning_move(+Winner, +Move, -WMove, -WWin, -BMove, -BWin)
% Updates the winning move based on the result of a move (the winner).
update_winning_move(Winner, Move, WMove, WWin, BMove, BWin):-
    Winner = 'p1', 
    WMove = Move, WWin = true, BMove = none, BWin = false.
update_winning_move(Winner, Move, WMove, WWin, BMove, BWin):-
    Winner = 'p2', 
    WMove = none, WWin = false, BMove = Move, BWin = true.
update_winning_move(_, _, WMove, WWin, BMove, BWin):-
    WMove = none, WWin = false, BMove = none, BWin = false.

% check_winner_1(+Winner, +Move, +NextWMove, +NextWWin, -WMove, -WWin)
% For the player 1, updates the winning move based on the result of a move.
check_winner_1(Winner, Move, _, _, WMove, WWin):-
    Winner = 'p1',  WMove = Move, WWin = true.
check_winner_1(_, _, NextWMove, NextWWin, WMove, WWin):-
    WMove = NextWMove, WWin = NextWWin.

% check_winner_2(+Winner, +Move, +NextBMove, +NextBWin, -BMove, -BWin)
% For the player 2, updates the winning move based on the result of a move.
check_winner_2(Winner, Move, _, _, BMove, BWin):-
    Winner = 'p2',  BMove = Move, BWin = true.
check_winner_2(_, _, NextBMove, NextBWin, BMove, BWin):-
    BMove = NextBMove, BWin = NextBWin.



% random_move(+GameState, -N, -X, -Y)
% Selects a random valid move (N, X, Y) for the given game state.
random_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y):-
    valid_moves([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves),
    random_member([N, X, Y], Moves).


% choose_move(+GameState, +Level, -Move)
% Takes the current game state and outputs the move selected by the computer player. 
% For Level 1, it generates a random valid move. 
% For Level 2, it determines the optimal move at the moment with a greedy algorithm, which checks if there is any possible winning move and evaluates the game state with the calculated value for the player.
choose_move(GameState, Level, Move):-
    choose_move_ratio(GameState, Level, Move, _), !.

% choose_move_ratio(+GameState, +Strategy, -Move, -MoveRatio)
% Gives a move chosen by the computer player, based on their strategy (1 for random valid move, 2 for best play).
choose_move_ratio([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N,X,Y], MoveRatio) :-
    random_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y),
    move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], NewGameState),
    ratio_value(NewGameState, MoveRatio).
choose_move_ratio([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, Move, MoveRatio) :-
    valid_moves([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, [WMove, _], [BMove, _]),
    get_move_ratio([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, WMove, BMove, Move, MoveRatio).

% get_move_ratio(+GameState, +Moves, +WMove, +BMove, -Move, -MoveRatio)
% Get the move and ratio based on the game state and moves.
get_move_ratio([Player, _, _, _, _, _, _, _, _], _, WMove, _, Move, MoveRatio):-
    Player = 'p1', WMove \= none,
    Move = WMove, MoveRatio = 1.
get_move_ratio([Player, _, _, _, _, _, _, _, _], _, _, BMove, Move, MoveRatio):-
    Player = 'p2', BMove \= none,
    Move = BMove, MoveRatio = 0.
get_move_ratio([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, _, _, Move, MoveRatio):-
    choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, BestMove, BestMoveRatio),
    Move = BestMove, MoveRatio = BestMoveRatio.

% choose_best_move(+GameState, +Moves, -BestMove, -MoveRatio)
% Finds the best move from a list of valid moves and calculates its ratio.
choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [Move], Move, MoveRatio):-
    move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, NewGameState), 
    ratio_value(NewGameState, MoveRatio).
choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [Move|Moves], BestMove, MoveRatio) :-
    move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, NewGameState), 
    ratio_value(NewGameState, NewMoveRatio),
    choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, OtherMove, OtherMoveRatio),
    choose_best_move_ratio(Player, Move, OtherMove, NewMoveRatio, OtherMoveRatio, BestMove, MoveRatio).

% choose_best_move_ratio(+Player, +Move, +OtherMove, +NewMoveRatio, +OtherMoveRatio, -BestMove, -MoveRatio)
% Compares two moves ratios and determines the best move for the current player.
choose_best_move_ratio(Player, Move, _, NewMoveRatio, OtherMoveRatio, BestMove, MoveRatio):-
    Player = 'p1', NewMoveRatio > OtherMoveRatio, 
    BestMove = Move, MoveRatio = NewMoveRatio.
choose_best_move_ratio(Player, Move, _, NewMoveRatio, OtherMoveRatio, BestMove, MoveRatio):-
    Player = 'p2', NewMoveRatio < OtherMoveRatio,
    BestMove = Move, MoveRatio = NewMoveRatio.
choose_best_move_ratio(_, _, OtherMove,  _, OtherMoveRatio, BestMove, MoveRatio):-
    BestMove = OtherMove, MoveRatio = OtherMoveRatio.
  