:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(gameOver). 
:- consult(board). 
:- consult(display). 
:- consult(colors).


% Menu ---------------------------------------------------------------------------------------------------------------------------
 
% state(+CurrentState, +Color1, +Color2)
% Handles the current state of the game and transitions to the next state based on user input or game logic

% Handles the initial state, allowing the user to choose to play, exit or change configurations
state(initial, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_menu(Color1, Color2), 
    read_option(3, Option),
    transition(initial, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle). 

% Handles the mode state, allowing the user to choose a game mode
state(mode, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_play,  
    read_option(3, Option),
    transition(mode, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle). 

% Handles the config state, allowing the user to change some configurations
state(config, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_config,  
    read_option(3, Option),
    transition(config, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle). 

% Handles the color state, allowing the user to choose custom colors for the game
state(colors, _, _, _, _):-
    print_banner_colors(1),
    print_banner_colors(2), nl,
    read_input_colors(Color11, Color22),
    print_banner_display_colors(Color11, Color22), nl,
    state(initial, Color11, Color22, _, _).

% Handles the board_size state, allowing the user to change the board size
state(board_size, Color1, Color2, _, BoardStyle):-
    print_banner_board_size,
    read_option(2, Option),
    state(initial, Color1, Color2, Option, BoardStyle). 

% Handles the board_style state, allowing the user to change the board style
state(board_style, Color1, Color2, BoardSize, _):-
    print_banner_board_style,
    read_option(3, Option),
    state(initial, Color1, Color2, BoardSize, Option). 


% Handles the state for a player vs computer game mode
state(play_uc, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_level,
    read_option(2, Option),
    transition(play_uc, Option, NextState), 
    state(NextState, Option, Color1, Color2, BoardSize, BoardStyle).

% Handles the state for a computer vs computer game mode
state(play_cc, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_pc,
    read_option(4, Option),
    transition(play_cc, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle).

% Handles the state when a winner is declared
state(winner, Color1, Color2, BoardSize, BoardStyle) :-
    read_option(2, Option),
    transition(winner, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle).

% Handles the exit state, where the game ends.
state(exit, _, _, _, _) :-
    write('Exiting...'), nl, nl. 

% state(+CurrentState, +LastOption, +Color1, +Color2)
% Handles the state where the player chooses whether they or the computer starts
state(play_uc_choose_start, LastOption, Color1, Color2, BoardSize, BoardStyle):-
    print_banner_starter,
    read_option(2, Option),
    transition(play_uc_choose_start, LastOption, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle).

% State handlers for different levels of the game
% Each level represents a specific game mode configuration
state(play_uu, Color1, Color2, BoardSize, BoardStyle) :- play_game('PlayerVsPlayer', Color1, Color2, BoardSize, BoardStyle).
state(levelUC11, Color1, Color2, BoardSize, BoardStyle) :- play_game('PlayerVsPc_1', Color1, Color2, BoardSize, BoardStyle).
state(levelUC12, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_1VsPlayer', Color1, Color2, BoardSize, BoardStyle).
state(levelUC21, Color1, Color2, BoardSize, BoardStyle) :- play_game('PlayerVsPc_2', Color1, Color2, BoardSize, BoardStyle).
state(levelUC22, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_2VsPlayer', Color1, Color2, BoardSize, BoardStyle).
state(levelCC11, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_1VsPc_1', Color1, Color2, BoardSize, BoardStyle).
state(levelCC12, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_1VsPc_2', Color1, Color2, BoardSize, BoardStyle).
state(levelCC21, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_2VsPc_1', Color1, Color2, BoardSize, BoardStyle).
state(levelCC22, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_2VsPc_2', Color1, Color2, BoardSize, BoardStyle).

% transition(+CurrentState, +Option, -NextState)
% Defines state transitions based on the current state and user input
transition(initial, 1, mode).  
transition(initial, 2, config).  
transition(initial, 3, exit).  
transition(mode, 1, play_uu).  
transition(mode, 2, play_uc).  
transition(mode, 3, play_cc).
transition(mode, 4, initial).
transition(config, 1, colors).
transition(config, 2, board_size).
transition(config, 3, board_style).
transition(play_uc, _, play_uc_choose_start).
transition(play_uc_choose_start, 1, 1, levelUC11).
transition(play_uc_choose_start, 1, 2, levelUC12).
transition(play_uc_choose_start, 2, 1, levelUC21).
transition(play_uc_choose_start, 2, 2, levelUC22).
transition(play_cc, 1, levelCC11).
transition(play_cc, 2, levelCC12).
transition(play_cc, 3, levelCC21).
transition(play_cc, 4, levelCC22).
transition(winner, 1, initial).
transition(winner, 2, exit).
transition(_, _, initial).


% Play -----------------------------------------------------------------------------------------------------------------------------

% play/0
% Opens the game menu and start the game cycle
play:- state(initial, white, white, 1, 1).


% play_game(+Mode, +Color1, +Color2, +BoardSize, +BoardStyle)
% Starts the game in the specified mode with given player colors, board size, and board style.
% It initiates the game state and the first turn.
play_game(Mode, Color1, Color2, BoardSize, BoardStyle):-
    initial_state(['p1', 'p2', Color1, Color2, BoardSize, BoardStyle], GameState),
    play_turn(Mode, GameState, 0.5).

% initial_state(+GameConfig, -GameState)
% Sets up the initial state of the game, generating the game state.
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
    handle_game(Mode, Winner, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], FinalRatio).

% final_ratio(+Winner, +MoveRatio, -NewMoveRatio)
% Determines a new ratio if there is a winner. 
final_ratio('p1', _, 1).
final_ratio('p2', _, 0).
final_ratio(_, MoveRatio, MoveRatio).

% handle_game(+Mode, +Winner, +GameState, +NewMoveRatio)
% Handles the current game state. 
% If there is a tie or a winner, the game ends. If not, it handles the next move.
handle_game(_, 'T', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], _) :- 
    write('Game tied!'), nl,
    state(initial, Color1, Color2, BoardSize, BoardStyle).
handle_game(_, Winner, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], _) :-
    Winner \= none,
    print_banner_final(Winner, Color1, Color2),
    state(winner, Color1, Color2, BoardSize, BoardStyle).
handle_game(Mode, _, State, MoveRatio) :-
    what_move(Mode, State, N, X, Y, NewMoveRatio, Exit),
    handle_move(Mode, State, [N, X, Y], Exit, NewMoveRatio).

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
    read_input(BoardSize, Levels, N, X, Y, Exit),
    player_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('PlayerVsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(BoardSize, Levels, N, X, Y, Exit),
    player_move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('PlayerVsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPlayer', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(BoardSize, Levels, N, X, Y, Exit),
    player_move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('Pc_1VsPlayer', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('PlayerVsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(BoardSize, Levels, N, X, Y, Exit),
    player_move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('PlayerVsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPlayer', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, Exit):-
    read_input(BoardSize, Levels, N, X, Y, Exit),
    player_move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], Exit, MoveRatio).

what_move('Pc_2VsPlayer', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_1', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_2', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y, MoveRatio, false):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, [N, X, Y], MoveRatio).


% player_move(+GameState, +Move, +Exit, -MoveRatio).
% Get the ratio based on the user player exiting the game or not.
player_move(_, _, true, MoveRatio) :-
    MoveRatio = 0.
player_move(GameState, Move, false, MoveRatio) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, MoveRatio).


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


% read_input(+BoardSize, +Levels, -Number, -X, -Y, -Exit)
% Reads the user input about their move or decision to exit the game.
read_input(BoardSize, Levels, N, X, Y, Exit) :-
    write('Choose the type of block (1-4) or type "exit" to give up: '),
    read_input_type(BoardSize, Levels, N, X, Y, Exit), nl.

% read_input_type(+BoardSize, +Levels, -Number, -X, -Y, -Exit)
% Reads the type of block input by the user
read_input_type(BoardSize, Levels, N, X, Y, Exit) :-
    read(InputN),
    validate_input_type(BoardSize, Levels, InputN, N, X, Y, Exit).

% validate_input_type(+BoardSize, +Levels, +InputN, -Number, -X, -Y, -Exit) 
% Validates the type of block input by the user or the choice to exit
validate_input_type(BoardSize, Levels, InputN, N, X, Y, false) :-
    integer(InputN), InputN >= 1, InputN =< 4,
    N = InputN,
    read_input_coordinates(BoardSize, Levels, X, Y), nl.
validate_input_type( _, _, 'exit', _, _, _, true).
validate_input_type(BoardSize, Levels, _, N, X, Y, Exit) :-
    write('Invalid. Choose a number between 1 and 4 or type exit.'),
    read_input_type(BoardSize, Levels, N, X, Y, Exit).


% read_input_coordinates(+BoardSize, +Levels, -X, -Y)
% Prompts the user to give X and Y coordinates of the move based on the board.
read_input_coordinates(BoardSize, Levels, X, Y) :-
    BoardSize = 1, 
    write('Choose the coordinate X (1-9): '), read(InputX),
    write('Choose the coordinate Y (1-9): '), read(InputY),
    validate_input_coordinates(BoardSize, Levels, InputX, InputY, X, Y).
read_input_coordinates(BoardSize, Levels, X, Y) :-
    BoardSize = 2,
    write('Choose the coordinate X (1-7): '), read(InputX),
    write('Choose the coordinate Y (1-7): '), read(InputY),
    validate_input_coordinates(BoardSize, Levels, InputX, InputY, X, Y).

% validate_input_coordinates(+BoardSize, +Levels, +InputX, +InputY, -X, -Y)
% Validates the X and Y coordinates entered by the user.
validate_input_coordinates(BoardSize, Levels, InputX, InputY, X, Y) :-
    \+ integer(InputX),
    write('The coordinates must be numbers'), nl, nl,
    read_input_coordinates(BoardSize, Levels, X, Y).
validate_input_coordinates(BoardSize, Levels, InputX, InputY, X, Y) :-
     \+ integer(InputY),
    write('The coordinates must be numbers'), nl, nl,
    read_input_coordinates(BoardSize, Levels, X, Y). 

validate_input_coordinates(BoardSize, Levels, InputX, InputY, X, Y) :-
    BoardSize = 1, (InputX < 1; InputX > 9; InputY < 1; InputY > 9), % é suposto ter uma condição para cada?
    write('The coordinates must be between 1 and 9.'), nl, nl,
    read_input_coordinates(BoardSize, Levels, X, Y). 

validate_input_coordinates(BoardSize, Levels, InputX, InputY, X, Y) :-
    BoardSize = 2, (InputX < 1; InputX > 7; InputY < 1; InputY > 7),
    write('The coordinates must be between 1 and 7.'), nl, nl,
    read_input_coordinates(BoardSize, Levels, X, Y). 

validate_input_coordinates(BoardSize, Levels, InputX, InputY, X, Y) :-
    validate_coordinates(InputX, InputY, BoardSize, Levels, Valid),
    Valid =:= 1,
    X = InputX, Y = InputY, !.

validate_input_coordinates(BoardSize, Levels, InputX, InputY, X, Y) :-
    validate_coordinates(InputX, InputY, BoardSize, Levels, Valid),
    Valid =:= 0, 
    write('Invalid coordinates. Try again.'), nl,
    read_input_coordinates(BoardSize, Levels, X, Y).


% Game ----------------------------------------------------------------------------------------------------------------------------

% valid_moves(+GameState, -ListOfMoves)
% Generates all valid moves for the current game state.
valid_moves([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], ListOfMoves) :-
    setof([N, X, Y], (
        generate_coordinates(1, 4, N),
        generate_coordinates(1, 9, X),
        generate_coordinates(1, 9, Y),
        validate_coordinates(X, Y, BoardSize, Levels, 1)
    ), ListOfMoves).

% generate_coordinates(+Low, +High, -Value)
% Generates all integers between Low and High.
generate_coordinates(Low, High, Low) :-
    Low =< High.
generate_coordinates(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    generate_coordinates(Next, High, Value).


% get_board_y(+BoardSize, +Y, -BoardY)
% Converts a Y to the corresponding board Y coordinate based on the board size.
get_board_y(1, Y, BoardY):- BoardY is 10 - Y.
get_board_y(2, Y, BoardY):- BoardY is 8 - Y.


% validate_coordinates(+X, +Y, +BoardSize, +Levels, -Valid)
% Validates if the coordinates (X, Y) can be placed on the board given the levels of the blocks and the board size.
validate_coordinates(X, Y, BoardSize, Levels, Valid) :-
    BoardX is X-1, get_board_y(BoardSize, Y, BoardY),
    BoardX2 is BoardX+1, BoardY2 is BoardY-1,

    1 is X mod 2, 1 is BoardY mod 2, 
    get_value(Levels, BoardY, BoardX, Level_LD), Level_LD is 0,
    get_value(Levels, BoardY, BoardX2, Level_LT), Level_LT is 0,
    get_value(Levels, BoardY2, BoardX, Level_RD), Level_RD is 0,
    get_value(Levels, BoardY2, BoardX2, Level_RT), Level_RT is 0,
    Valid is 1.

validate_coordinates(X, Y, BoardSize, Levels, Valid) :-
    BoardX is X-1, get_board_y(BoardSize, Y, BoardY),
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


% get_new_y(+BoardSize,  +Y, -NewY)
% Converts the given Y to a new value Y coordinate based on the board size.
get_new_y(1, Y, NewY):- NewY is 10 - Y.
get_new_y(2, Y, NewY):- NewY is 8 - Y.

% move(+GameState, +Move, -NewGameState)
% Executes a move by placing a block of type N at coordinates (X, Y) on the board, if the move is valid.
move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], [OtherPlayer, Board4, Levels4, Player, Moves1, Color1, Color2, BoardSize, BoardStyle]):-
    Moves1 is MovesLeft-1,
    NewX is X, get_new_y(BoardSize, Y, NewY),

    validate_coordinates(X, Y, BoardSize, Levels, 1),  % Check if its valid

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
value([_, _, _, _, 0, _, _, _, _], 0.5).
value([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], ClampedValue):-
    valid_moves([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, [WMove, _], [BMove, _]),
    get_compension(WMove, BMove, Player, Compensation),
    get_value(WMove, BMove, Player, Board, Compensation, Value),
    ClampedValue is max(0, min(1, Value)).


%get_compension(WMove, BMove, Player, Compensation)
get_compension(WMove, _, Player, Compensation):-
    WMove \= none, Player = 'p2',
    Compensation = 0.15.
get_compension(_, BMove, Player, Compensation):-
    BMove \= none, Player = 'p1',
    Compensation = -0.15.
get_compension(_, _, _ ,Compensation):-
    Compensation = 0.

% get_value(WMove, BMove, Player, Value)
get_value(WMove, _, Player, _, _, Value):-
    WMove \= none, Player = 'p1',
    Value = 1.
get_value(_, BMove, Player, _, _, Value):-
    BMove \= none, Player = 'p2',
    Value = 0.
get_value(_, _, Player, Board, Compensation, Value):-
    NumRC is 1, get_block(Player, Block, OtherBlock),
    max_count_col(Board, NumRC, Block, CountColGood, _),
    max_count_row(Board, NumRC, OtherBlock, CountRowBad, _),

    TotalCount is CountColGood + CountRowBad,
    BaseValue is CountColGood / TotalCount,

    Value is BaseValue + Compensation.

%get_block(Player, Block, OtherBlock)
get_block('p1', 'W', 'B').
get_block('p2', 'B', 'W').


max_count([], _, _, CurrentMax, CurrentRow, CurrentMax, CurrentRow).
max_count([Row | Rest], NumRow, Block, CurrentMax, CurrentRow, Max, MaxRow):-
    count_block(Row, Block, CountRow),
    update_max(CountRow, CurrentMax, NumRow, CurrentRow, NewMax, NewMaxRow),
    NextRow is NumRow + 1,
    max_count(Rest, NextRow, Block, NewMax, NewMaxRow, Max, MaxRow).

count_block([], _, 0).
count_block([Elem | Rest], Block, Count):-
    count_block(Rest, Block, RestCount),
    Elem = Block, Count is RestCount + 1.
count_block([Elem | Rest], Block, Count):-
    count_block(Rest, Block, RestCount),
    Count is RestCount.

update_max(CountRow, CurrentMax, NumRow, _, NewMax, NewMaxRow) :-
    CountRow > CurrentMax,
    NewMax = CountRow, NewMaxRow = NumRow.
update_max(CountRow, CurrentMax, _, CurrentRow, NewMax, NewMaxRow) :-
    CountRow =< CurrentMax,
    NewMax = CurrentMax, NewMaxRow = CurrentRow.

max_count_row(Board, NumRC, Block, CountRow, NumRow):-
    max_count(Board, NumRC, Block, -1, -1, CountRow, NumRow).

max_count_col(Board, NumRC, Block, CountCol, NumCol):-
    transpose(Board, TransposedBoard),
    max_count(TransposedBoard, NumRC, Block, -1, -1, CountCol, NumCol).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% is_any_winning_move(+GameState, +Moves, -WhiteWinningMove, -BlackWinningMove)
% Checks if any move in the list of Moves is a winning move for either player, and if there is saves it.
is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [Move], [WMove, WWin], [BMove, BWin]):-
    winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, Winner),
    update_winning_move(Winner, Move, WMove, WWin, BMove, BWin).
is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [Move|Moves], [WMove, WWin], [BMove, BWin]):-
    winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, Winner),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Moves, [NextWMove, NextWWin], [NextBMove, NextBWin]),
    check_winner_1(Winner, Move, NextWMove, NextWWin, WMove, WWin),
    check_winner_2(Winner, Move, NextBMove, NextBWin, BMove, BWin).

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
update_winning_move(Winner, Move, WMove, WWin, BMove, BWin):-
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

% choose_move(+GameState, +Strategy, -Move, -MoveRatio)
% Gives a move chosen by the computer player, based on their strategy (1 for random valid move, 2 for best play).
choose_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 1, [N,X,Y], MoveRatio) :-
    random_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], N, X, Y),
    move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [N, X, Y], NewGameState),
    value(NewGameState, MoveRatio).
choose_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], 2, Move, MoveRatio) :-
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
    value(NewGameState, MoveRatio).
choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], [Move|Moves], BestMove, MoveRatio) :-
    move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle], Move, NewGameState), 
    value(NewGameState, NewMoveRatio),
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
  
