:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(gameOver). 
:- consult(board). 
:- consult(display). 
:- consult(colors).


% Menu ------------------------------------------------------------------------------------------------
state(initial, Color1, Color2) :-
    print_banner(menu), 
    read_option(Option, 3),
    transition(initial, Option, NextState), 
    state(NextState, Color1, Color2). 

state(mode, Color1, Color2) :-
    print_banner(play),  
    read_option(Option, 3),
    transition(mode, Input, NextState), 
    state(NextState, Color1, Color2). 

state(colors, Color1, Color2):-
    print_banner(colors, 1),
    print_banner(colors, 2),  
    read_input_colors(Color11, Color22),
    print_banner(display_colors, Color11, Color22),
    state(initial, Color11, Color22).


state(play_uu, Color1, Color2) :-
    %print_banner(play, )
    play_game('PlayerVsPlayer', Color1, Color2).

state(play_uc, Color1, Color2) :-
    print_banner(level, 0),
    read_option(Option, 2),
    transition(play_uc, Input, NextState), 
    state(NextState, Color1, Color2).
state(level_1, Color1, Color2) :-
    play_game('PlayerVsPc_1', Color1, Color2).
state(level_2, Color1, Color2) :-
    play_game('PlayerVsPc_2', Color1, Color2).

state(play_cc, Color1, Color2) :-
    play_game('PcVsPc', Color1, Color2).

state(exit, Color1, Color2) :-
    write('Exiting...'), nl, nl. 


transition(initial, 1, mode).  
transition(initial, 2, colors).  
transition(initial, 3, exit).  
transition(mode, 1, play_uu).  
transition(mode, 2, play_uc).  
transition(mode, 3, play_cc).
transition(mode, 4, initial).
transition(play_uc, 1, level_1).
transition(play_uc, 2, level_2).
transition(play_cc, 1, level_11).
transition(play_cc, 2, level_12).
transition(play_cc, 3, level_21).
transition(play_cc, 4, level_22).
transition(_, _, initial). 


% Play ------------------------------------------------------------------------------------------------

initial_state([Player, OtherPlayer], [Player, Board, Levels, OtherPlayer, 54]).

play :- state(initial, white, white).

choose_players('PlayerVsPlayer', ['p1','p2']).
choose_players('PlayerVsPc_1', ['p1','p2']).
choose_players('PlayerVsPc_2', ['p1','p2']).
choose_players('PcVsPc', ['p1','p2']).

play_game(Mode, Color1, Color2):-
    board(B), levels(L),
    choose_players(Mode, Players),
    initial_state(Players, [P1, B, L, P2, MovesLeft]),
    play_turn(Mode, [P1, B, L, P2, MovesLeft, Color1, Color2]).

play_turn('PlayerVsPlayer', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value([Player, Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game([Player, Board, Levels, Color1, Color2, Ratio]),  
    game_over([Player, Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        read_input(N,X,Y, Levels, Color1, Color2),
        move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, X, Y], NewState),
        play_turn('PlayerVsPlayer', NewState)
    ).

play_turn('PlayerVsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value([Player, Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game(['p1', Board, Levels, Color1, Color2, Ratio]),  
    game_over(['p1', Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        read_input(N,X,Y, Levels, Color1, Color2),
        move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, X, Y], NewState),
        play_turn('PlayerVsPc_1', NewState)
    ).

play_turn('PlayerVsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value([Player, Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game(['p2', Board, Levels, Color1, Color2, Ratio]),  
    game_over(['p2', Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner

        random_move(Board, Levels, N, X, Y),
        move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, X, Y], NewState),
        play_turn('PlayerVsPc_1', NewState)
    ).

play_turn('PlayerVsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value(['p1', Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game(['p1', Board, Levels, Color1, Color2, Ratio]),  
    game_over(['p1', Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        read_input(N,X,Y, Levels, Color1, Color2),
        move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, X, Y], NewState),
        play_turn('PlayerVsPc_2', NewState)
    ).

play_turn('PlayerVsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value(['p2', Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game(['p2', Board, Levels, Color1, Color2, Ratio]),  
    game_over(['p2', Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        Board2 = Board,
        choose_move(['p2', Board2, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y]),
        move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, X, Y], NewState),
        play_turn('PlayerVsPc_2', NewState)
    ).


play_turn('PcVsPc', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value([Player, Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game([Player, Board, Levels, Color1, Color2, Ratio]),  
    game_over([Player, Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        random_move(Board,Levels, N, X, Y),
        move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, X, Y], NewState),
        play_turn('PcVsPc', NewState)
    ).



% Validate Inputs --------------------------------------------------------------------------------------
read_option(Option, N) :-
    write('Option: '), nl,
    read(Input),
    (integer(Input), Input > 0, Input =< N -> Option is Input, nl; write('Invalid input. '), read_option(Option, N)).


read_input(N, X, Y, Levels, Color1, Color2) :-
    display_pieces(Color1, Color2),
    write('Choose the type of block (1-4): '),
    validate_input_type(N), nl,
    read_input_coordinates(X, Y, Levels), nl.


validate_input_type(N) :-
    read(InputN),
    integer(InputN), InputN >= 1, InputN =< 4, N = InputN, !.
validate_input_type(N) :-
    write('Invalid. Choose a number between 1 and 4. '),
    validate_input_type(N).


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
    validate_coordinates(X, Y, Levels). 

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
    findall([N, X, Y], (
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


value([Player, Board, Levels, OtherPlayer, MovesLeft], Value):-
    NumRC is 1,
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer; WhiteWinner = OtherPlayer, BlackWinner = Player),
    (Player = 'p1' -> Block = 'W', OtherBlock = 'B'; Block = 'B', OtherBlock = 'W'),
    %valid_moves(Board, Levels, Moves),
    %is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Player, Moves, MoveWinnerPlayer),
    %is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], OtherPlayer, Moves, MoveWinnerOtherPlayer),
    %( MoveWinnerPlayer \= none, Player = WhiteWinner ->                  
    %    Value = 1 
    %;   
    %    MoveWinnerPlayer \= none, Player = BlackWinner ->                  
    %    Value = 0 
    %; 
    %    MoveWinnerOtherPlayer \= none, OtherPlayer = WhiteWinner ->                  
    %    Value = 1 
    %;   
    %    MoveWinnerOtherPlayer \= none, OtherPlayer = BlackWinner ->                  
    %    Value = 0 
    %; 
        %max_count_row(Board, NumRC, Block, CountRowGood, NumRowGood),
        max_count_col(Board, NumRC, Block, CountColGood, NumColGood),
        max_count_row(Board, NumRC, OtherBlock, CountRowBad, NumRowBad),
        %max_count_col(Board, NumRC, OtherBlock, CountColBad, NumColBad),
        TotalCount is CountColGood + CountRowBad,
        Value is CountColGood / TotalCount.
    %).    

is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], PlayerWanted,[Move], MoveWinner):-
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer; WhiteWinner = OtherPlayer, BlackWinner = Player),
    winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, Winner),
    (
        Winner = PlayerWanted ->
            MoveWinner = Move                  
    ;

        MoveWinner = none
    ).    

is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], PlayerWanted,[Move|Moves], MoveWinner):-
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer; WhiteWinner = OtherPlayer, BlackWinner = Player),
    winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, Winner),
    (
        Winner = PlayerWanted ->
            MoveWinner = Move                  
    ;
        is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], PlayerWanted,Moves, MoveWinner)
    ).    
    
winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, Winner):-
    NumRC is 1,
    check_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, OtherGameState),
    game_over(OtherGameState, Winner).

block_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], BlockerMove) :-
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer ; WhiteWinner = OtherPlayer, BlackWinner = Player),
    findall([Value, [N2, X, Y]], (
        generate_coordinates(1, 4, N2),
        \+ N2 = N,
        check_move([Player, Board, Levels, OtherPlayer, MovesLeft], [N2, X, Y], NewGameState),
        value(NewGameState, Value)
    ), MovesWithValues),
    write(MovesWithValues),nl,
    (Player = WhiteWinner ->
        max_member([_, BlockerMove], MovesWithValues) 
    ;
        min_member([_, BlockerMove], MovesWithValues) 
    ).


choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 2, Move) :-
    valid_moves(Board, Levels, Moves),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Player, Moves, MoveWinner),
    write(Player), nl,
    (MoveWinner \= none ->
        write('awdwadawda'),nl,
        Move = MoveWinner
    ;
        write('awdawd'),nl,
        is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], OtherPlayer, Moves, MoveWinnerOther),
        (MoveWinnerOther \= none -> 
            block_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], MoveWinnerOther, BlockerMove),
            Move = BlockerMove
        ;
            choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, BestMove),
            Move = BestMove
        )
    ),
    nl, write(Move), nl.


choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], [Move], Move).

choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], [Move|Moves], BestMove) :-
    check_move([Player, Board, Levels, OtherPlayer, MovesLeft], Move, NewGameState), 
    value(NewGameState, Value),
    write(Move), write(Value),
    choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, OtherMove),
    check_move([Player, Board, Levels, OtherPlayer, MovesLeft], OtherMove, OtherGameState),
    value(OtherGameState, OtherValue),
    (Value < OtherValue -> BestMove = Move; BestMove = OtherMove).   
    



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
    NewX is 1+(X-1)*2, NewY is 10 - Y,

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