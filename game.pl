:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(gameOver). 
:- consult(board). 
:- consult(display). 
:- consult(colors).


% Menu ------------------------------------------------------------------------------------------------
state(initial, Color1, Color2) :-
    print_banner_menu(Color1, Color2), 
    read_option(Option, 3),
    transition(initial, Option, NextState), 
    state(NextState, Color1, Color2). 

state(mode, Color1, Color2) :-
    print_banner_play,  
    read_option(Option, 3),
    transition(mode, Option, NextState), 
    state(NextState, Color1, Color2). 

state(colors, Color1, Color2):-
    print_banner_colors(1),
    print_banner_colors(2), nl,
    read_input_colors(Color11, Color22),
    print_banner_display_colors(Color11, Color22), nl,
    state(initial, Color11, Color22).

state(play_uu, Color1, Color2) :-
    play_game('PlayerVsPlayer', Color1, Color2).

state(play_uc, Color1, Color2) :-
    print_banner_level,
    read_option(Option, 2),
    transition(play_uc, Option, NextState), 
    state(NextState, Option, Color1, Color2).

state(play_uc_choose_start, LastOption, Color1, Color2):-
    print_banner_starter,
    read_option(Option, 2),
    transition(play_uc_choose_start, LastOption, Option, NextState), 
    state(NextState, Color1, Color2).

state(play_cc, Color1, Color2) :-
    print_banner_pc,
    read_option(Option, 4),
    transition(play_cc, Option, NextState), 
    state(NextState, Color1, Color2).

state(levelUC11, Color1, Color2) :-
    play_game('PlayerVsPc_1', Color1, Color2).

state(levelUC12, Color1, Color2) :-
    play_game('Pc_1VsPlayer', Color1, Color2).

state(levelUC21, Color1, Color2) :-
    play_game('PlayerVsPc_2', Color1, Color2).

state(levelUC22, Color1, Color2) :-
    play_game('Pc_2VsPlayer', Color1, Color2).

state(levelCC11, Color1, Color2) :-
    play_game('Pc_1VsPc_1', Color1, Color2).

state(levelCC12, Color1, Color2) :-
    play_game('Pc_1VsPc_2', Color1, Color2).

state(levelCC21, Color1, Color2) :-
    play_game('Pc_2VsPc_1', Color1, Color2).

state(levelCC22, Color1, Color2) :-
    play_game('Pc_2VsPc_2', Color1, Color2).

state(winner, Color1, Color2) :-
    read_option(Option, 2),
    transition(winner, Option, NextState), 
    state(NextState, Color1, Color2).

state(exit, Color1, Color2) :-
    write('Exiting...'), nl, nl. 


transition(initial, 1, mode).  
transition(initial, 2, colors).  
transition(initial, 3, exit).  
transition(mode, 1, play_uu).  
transition(mode, 2, play_uc).  
transition(mode, 3, play_cc).
transition(mode, 4, initial).
transition(play_uc, N, play_uc_choose_start).
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


% Play ------------------------------------------------------------------------------------------------

initial_state([Player, OtherPlayer], [Player, Board, Levels, OtherPlayer, 54]):-
    board(Board), levels(Levels).

play :- state(initial, white, white).

play_game(Mode, Color1, Color2):-
    initial_state(['p1','p2'], [P1, Board, Levels, P2, MovesLeft]),
    play_turn(Mode, [P1, Board, Levels, P2, MovesLeft, Color1, Color2], 0.5).


play_turn(Mode, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], MoveRatio) :- 
    game_over([Player, Board, Levels, OtherPlayer, MovesLeft], Winner),  
    (Winner = 'p1' -> FinalRatio = 1 ; Winner = 'p2' -> FinalRatio = 0 ; FinalRatio = MoveRatio),
    display_game([Player, Board, Levels, Color1, Color2, FinalRatio, MovesLeft]),
    ( Winner = 'T' ->                  
        write('Game tied!'),nl,
        state(initial, Color1, Color2)
    ;   
        Winner \= none ->                  
        %format("~w venceu o jogo!~n", [Winner]),nl,
        print_banner_final(Winner, Color1, Color2),
        state(winner, Color1, Color2)
    ;
        what_move(Mode, [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, NewMoveRatio, Exit),
        (
        Exit = false ->
            move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, X, Y], NewState),
            play_turn(Mode, NewState, NewMoveRatio)
        ;
            print_banner_final(OtherPlayer, Color1, Color2),
            state(winner, Color1, Color2)
        )
    ).

what_move('PlayerVsPlayer', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit),
    (Exit = true -> MoveRatio = 0
    ;
        check_move([Player, Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], NewGameState),
        value(NewGameState, MoveRatio)
    ).

    

what_move('PlayerVsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit),
    (Exit = true -> MoveRatio = 0
    ;
        check_move(['p1', Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], NewGameState),
        value(NewGameState, MoveRatio)
    ).

what_move('PlayerVsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPlayer', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit),
    (Exit = true -> MoveRatio = 0
    ;
        check_move(['p2', Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], NewGameState),
        value(NewGameState, MoveRatio)
    ).

what_move('Pc_1VsPlayer', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y], MoveRatio).

what_move('PlayerVsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit),
    (Exit = true -> MoveRatio = 0
    ;
        check_move(['p1', Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], NewGameState),
        value(NewGameState, MoveRatio)
    ).

what_move('PlayerVsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move(['p2', Board, Levels, 'p1', MovesLeft], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPlayer', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, Exit):-
    read_input(N,X,Y, Levels, Color1, Color2, Exit),
    (Exit = true -> MoveRatio = 0
    ;
        check_move(['p2', Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], NewGameState),
        value(NewGameState, MoveRatio)
    ).


what_move('Pc_2VsPlayer', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_1', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y], MoveRatio).

what_move('Pc_1VsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move(['p1', Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move(['p2', Board, Levels, OtherPlayer, MovesLeft], 1, [N, X, Y], MoveRatio).

what_move('Pc_2VsPc_2', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], N, X, Y, MoveRatio, false):-
    choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y], MoveRatio).



% Validate Inputs --------------------------------------------------------------------------------------
read_option(Option, N) :-
    write('Option: '), nl,
    read(Input),
    (integer(Input), Input > 0, Input =< N -> Option is Input, nl; write('Invalid input. '), read_option(Option, N)).


read_input(N, X, Y, Levels, Color1, Color2, Exit) :-
    write('Choose the type of block (1-4) or type "exit" to give up: '),
    validate_input_type(N, Exit2), nl, Exit = Exit2,
    (Exit2 = false -> read_input_coordinates(X, Y, Levels), nl; Exit2 = true -> !).


validate_input_type(N, Exit) :-
    read(InputN),
    (integer(InputN), InputN >= 1, InputN =< 4 -> N = InputN, Exit = false
    ;
     InputN = 'exit' -> Exit = true
    ), !.

validate_input_type(N, Exit) :-
    write('Invalid. Choose a number between 1 and 4 or type exit.'),
    validate_input_type(N, Exit).


read_input_coordinates(X, Y, Levels) :-
    write('Choose the coordinate X (1-9): '), read(InputX),
    write('Choose the coordinate Y (1-9): '), read(InputY),
    validate_input_coordinates(InputX, InputY, X, Y, Levels).

validate_input_coordinates(InputX, InputY, X, Y, Levels) :-
    (\+ integer(InputX); \+ integer(InputY)),
    write('The coordinates must be numbers'), nl, nl,
    validate_coordinates(X, Y, Levels). 

validate_input_coordinates(InputX, InputY, X, Y, Levels) :-
    (InputX < 1; InputX > 9; InputY < 1; InputY > 9),
    write('The coordinates must be between 1 and 9.'), nl, nl,
    read_input_coordinates(X, Y, Levels). 

validate_input_coordinates(InputX, InputY, X, Y, Levels) :-
    validate_coordinates(InputX, InputY, Levels, Valid),
    ( Valid =:= 1 ->
        X = InputX, Y = InputY, !
    ; 
        Valid =:= 0 ->
        write('Invalid coordinates. Try again.'), nl,
        read_input_coordinates(X, Y, Levels)
    ).

% Game ------------------------------------------------------------------------------------------
valid_moves(Board, Levels, Moves) :-
    setof([N, X, Y], (
        generate_coordinates(1, 4, N),
        generate_coordinates(1, 9, X),
        generate_coordinates(1, 9, Y),
        validate_coordinates(X, Y, Levels, 1)
    ), Moves).

generate_coordinates(Low, High, Low) :-
    Low =< High.
generate_coordinates(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    generate_coordinates(Next, High, Value).

random_index(Low, High, Index) :-
    random(X),
    Index is Low + floor(X * (High - Low)).

random_move(Board, Levels, N, X, Y):-
    valid_moves(Board, Levels, Moves),
    random_member([N, X, Y], Moves).



validate_coordinates(X, Y, Levels, Valid) :-
    BoardX is X-1, BoardY is 10 - Y, 
    BoardX2 is BoardX+1, BoardY2 is BoardY-1,

    1 is X mod 2, 1 is BoardY mod 2, 
    get_value(Levels, BoardY, BoardX, Level_LD), Level_LD is 0,
    get_value(Levels, BoardY, BoardX2, Level_LT), Level_LT is 0,
    get_value(Levels, BoardY2, BoardX, Level_RD), Level_RD is 0,
    get_value(Levels, BoardY2, BoardX2, Level_RT), Level_RT is 0,
    %write('Level 0.'), nl, nl, 
    Valid is 1.

validate_coordinates(X, Y, Levels, Valid) :-
    BoardX is X-1, BoardY is 10 - Y, 
    BoardX2 is BoardX+1, BoardY2 is BoardY-1,

    get_value(Levels, BoardY, BoardX, L1),
    get_value(Levels, BoardY, BoardX2, L2),
    get_value(Levels, BoardY2, BoardX, L3),
    get_value(Levels, BoardY2, BoardX2, L4),
    L2 is L1, L3 is L1, L4 is L1,

    ((0 is L1 mod 2, 1 is X mod 2, 1 is Y mod 2) ; (1 is L1 mod 2, 0 is X mod 2, 0 is Y mod 2)),

    %write('Above levels.'), nl, nl, 
    Valid is 1.

validate_coordinates(X, Y, Levels, Valid) :-
    Valid is 0.


% Auxiliar functions
get_first([Head| _], Head).
remove_first([_ | Tail], Tail).

% não sei se posso usar a do module list 
transposee([], []).
transposee([[] | _], []).
transposee(Board, [Row | Transposed]):-
    maplist(get_first, Board, Row),
    maplist(remove_first, Board, RestRows),
    transposee(RestRows, Transposed).


%%%%%%%%%%%%%%%%%%%%%%%
count_block([], _, 0).
count_block([Elem | Rest], Block, Count):-
    count_block(Rest, Block, RestCount),
    (Elem = Block -> Count is RestCount + 1 ; Count is RestCount).

max_count([], _, _, CurrentMax, CurrentRow, CurrentMax, CurrentRow).
max_count([Row | Rest], NumRow, Block, CurrentMax, CurrentRow, Max, MaxRow):-
    count_block(Row, Block, CountRow),
    (CountRow > CurrentMax -> NewMax = CountRow, NewMaxRow = NumRow;
                              NewMax = CurrentMax, NewMaxRow = CurrentRow),
    NextRow is NumRow + 1,
    max_count(Rest, NextRow, Block, NewMax, NewMaxRow, Max, MaxRow).


max_count_row(Board, NumRC, Block, CountRow, NumRow):-
    max_count(Board, NumRC, Block, -1, -1, CountRow, NumRow).

max_count_col(Board, NumRC, Block, CountCol, NumCol):-
    transposee(Board, TransposedBoard),
    max_count(TransposedBoard, NumRC, Block, -1, -1, CountCol, NumCol).

value([Player, Board, Levels, OtherPlayer, 0], 0.5).
    
value([Player, Board, Levels, OtherPlayer, MovesLeft], ClampedValue):-
    NumRC is 1,
    (Player = 'p1' -> Block = 'W', OtherBlock = 'B'; Block = 'B', OtherBlock = 'W'),
    valid_moves(Board, Levels, Moves),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, [WMove, WWin], [BMove, BWin]),
    (  WMove \= none, Player = 'p2' ->
        Compensation = 0.15
    ; BMove \= none, Player = 'p1' ->
        Compensation = -0.15
    ; 
        Compensation = 0
    ),

    ( WMove \= none, Player = 'p1' ->                  
        Value = 1 
    ;   
        BMove \= none, Player = 'p2' ->                  
        Value = 0 
    ; 
        max_count_col(Board, NumRC, Block, CountColGood, NumColGood),
        max_count_row(Board, NumRC, OtherBlock, CountRowBad, NumRowBad),

        TotalCount is CountColGood + CountRowBad,
        BaseValue is CountColGood / TotalCount,
    
        Value is BaseValue + Compensation
    ),
    ClampedValue is max(0, min(1, Value)).


is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], [Move], [WMove, WWin], [BMove, BWin]):-
    winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, Winner),
    (
        Winner = 'p1' ->
            WMove = Move,
            WWin = true,
            BMove = none,
            BWin = false
    ;
        Winner = 'p2' ->
            WMove = none,
            WWin = false,
            BMove = Move,
            BWin = true
    ;
        WMove = none,
        WWin = false,
        BMove = none,
        BWin = false
    ).

is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], [Move|Moves], [WMove, WWin], [BMove, BWin]):-
    winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, Winner),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, [NextWMove, NextWWin], [NextBMove, NextBWin]),
    (
        Winner = 'p1' ->
            WMove = Move,
            WWin = true
    ;
        WMove = NextWMove,
        WWin = NextWWin
    ),
    (
        Winner = 'p2' ->
            BMove = Move,
            BWin = true
    ;
        BMove = NextBMove,
        BWin = NextBWin
    ).

winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, Winner):-
    check_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, OtherGameState),
    game_over(OtherGameState, Winner).


choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 1, [N,X,Y], MoveRatio) :-
    random_move(Board, Levels, N, X, Y),
    check_move([Player, Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], NewGameState),
    value(NewGameState, MoveRatio).

choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 2, Move, MoveRatio) :-
    valid_moves(Board, Levels, Moves),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, [WMove, WWin], [BMove, BWin]),
    (
        Player = 'p1', WMove \= none ->
        Move = WMove,
        MoveRatio = 1
    ;
        Player = 'p2', BMove \= none ->
        Move = BMove,
        MoveRatio = 0

    ;
        choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, BestMove, BestMoveRatio),
        Move = BestMove,
        MoveRatio = BestMoveRatio
    ).

choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], [Move], Move, MoveRatio):-
    check_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, NewGameState), 
    value(NewGameState, MoveRatio).

choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], [Move|Moves], BestMove, MoveRatio) :-
    check_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, NewGameState), 
    value(NewGameState, NewMoveRatio),
    choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, OtherMove, OtherMoveRatio),
    (Player = 'p1', NewMoveRatio > OtherMoveRatio -> BestMove = Move, MoveRatio = NewMoveRatio
    ; 
    Player = 'p2', NewMoveRatio < OtherMoveRatio -> BestMove = Move, MoveRatio = NewMoveRatio
    ; 
        BestMove = OtherMove, MoveRatio = OtherMoveRatio
    ).
    

move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, X, Y], [OtherPlayer, Board4, Levels4, Player, Moves1, Color1, Color2]):-
    Moves1 is MovesLeft-1,
    NewX is X, NewY is 10 - Y,

    % Get block
    piece_from_number(N, Piece),
    piece_coordinates(Piece, PieceConfig), 
    get_value(PieceConfig, 0, 0, V0),
    get_value(PieceConfig, 0, 1, V1),
    get_value(PieceConfig, 1, 0, V2),
    get_value(PieceConfig, 1, 1, V3),

    % Update board
    get_value(Board, NewY, NewX, Value), % Para que serve isto?
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


check_move([Player, Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], [OtherPlayer, Board4, Levels4, Player, Moves1]):-
    Moves1 is MovesLeft-1,
    NewX is X, NewY is 10 - Y,

    % Get block
    piece_from_number(N, Piece),
    piece_coordinates(Piece, PieceConfig), 
    get_value(PieceConfig, 0, 0, V0),
    get_value(PieceConfig, 0, 1, V1),
    get_value(PieceConfig, 1, 0, V2),
    get_value(PieceConfig, 1, 1, V3),

    % Update board
    get_value(Board, NewY, NewX, Value), % Para que serve isto?
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