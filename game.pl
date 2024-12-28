:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(gameOver). 
:- consult(board). 
:- consult(display). 
:- consult(colors).

% Menu
state(initial, Color1, Color2) :-
    print_banner(30, '0'),  
    write('Option: '), nl,
    read(Input),
    transition(initial, Input, NextState), 
    state(NextState, Color1, Color2). 

state(mode, Color1, Color2) :-
    print_bannerPlay(30, '0'),  
    write('Option: '), nl,
    read(Input),
    transition(mode, Input, NextState), 
    state(NextState, Color1, Color2). 

state(colors, Color1, Color2):-
    print_bannerColors(30, '0'),  
    read_input_colors(Color11, Color22),
    display_code('S', Color11),
    display_code('S', Color22),
    state(initial, Color11, Color22). 


state(play_uu, Color1, Color2) :-
    play_game('PlayerVsPlayer', Color1, Color2).


state(play_uc, Color1, Color2) :-
    %FALTA PRINT BANNER
    write('Option: '), nl,
    read(Input),
    transition(play_uc, Input, NextState), 
    state(NextState, Color1, Color2). 
state(level_1, Color1, Color2) :-
    play_game('PlayerVsPc_1', Color1, Color2).
state(level_2, Color1, Color2) :-
    play_game('PlayerVsPc_2', Color1, Color2).

state(play_cc, Color1, Color2) :-
    play_game('PcVsPc', Color1, Color2).

state(exit) :-
    write('Exiting...'), nl. 

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_state([Player, OtherPlayer], [Player, Board, Levels, OtherPlayer, 54]).

play :- state(initial, red, blue).

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
    display_game([Player, Board, Color1, Color2, Ratio]),  
    game_over([Player, Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        read_input(N,X,Y, Levels, Color1, Color2),
        move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, Y, X], NewState),
        play_turn('PlayerVsPlayer', NewState)
    ).

play_turn('PlayerVsPc_1', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value([Player, Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game(['p1', Board, Color1, Color2, Ratio]),  
    game_over(['p1', Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        read_input(N,X,Y, Levels, Color1, Color2),
        move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, Y, X], NewState),
        play_turn('PlayerVsPc_1', NewState)
    ).

play_turn('PlayerVsPc_1', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value([Player, Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game(['p2', Board, Color1, Color2, Ratio]),  
    game_over(['p2', Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner

        random_move(Board, Levels, N, X, Y),
        move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, Y, X], NewState),
        play_turn('PlayerVsPc_1', NewState)
    ).

play_turn('PlayerVsPc_2', ['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value(['p1', Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game(['p1', Board, Color1, Color2, Ratio]),  
    game_over(['p1', Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        read_input(N,X,Y, Levels, Color1, Color2),
        move(['p1', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, Y, X], NewState),
        play_turn('PlayerVsPc_2', NewState)
    ).

play_turn('PlayerVsPc_2', ['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value(['p2', Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game(['p2', Board, Color1, Color2, Ratio]),  
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
        move(['p2', Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, Y, X], NewState),
        play_turn('PlayerVsPc_2', NewState)
    ).


play_turn('PcVsPc', [Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2]) :-
    value_next_move([Player, Board, Levels, OtherPlayer, MovesLeft], Ratio),
    display_game([Player, Board, Color1, Color2, Ratio]),  
    game_over([Player, Board, Levels, OtherPlayer, MovesLeft], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 2, [N, X, Y]),
        move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, Y, X], NewState),
        play_turn('PcVsPc', NewState)
    ).



% Validate Inputs
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


% Game
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
    BoardX is 1+(X-1)*2, BoardY is 10 - Y, 
    BoardX2 is BoardX+2, BoardY2 is BoardY-1,

    1 is X mod 2, 1 is BoardY mod 2, 
    get_value(Levels, BoardY, BoardX, Level_LD), %write( Level_LD), nl, 
    Level_LD is 0,
    get_value(Levels, BoardY, BoardX2, Level_LT), %write( Level_LT), nl, 
    Level_LT is 0,
    get_value(Levels, BoardY2, BoardX, Level_RD), %write( Level_RD), nl, 
    Level_RD is 0,
    get_value(Levels, BoardY2, BoardX2, Level_RT), %write( Level_RT), nl, 
    Level_RT is 0,
    %write('Level 0.'), nl, nl, 
    Valid is 1.

validate_coordinates(X, Y, Levels, Valid) :-
    BoardX is 1+(X-1)*2, BoardY is 10 - Y,
    BoardX2 is BoardX+2, BoardY2 is BoardY-1,

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
    valid_moves(Board, Levels, Moves),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Player, Moves, MoveWinnerPlayer),
    ( MoveWinnerPlayer \= none, Player = WhiteWinner ->                  
        Value = 1 
    ;   
        MoveWinnerPlayer \= none, Player = BlackWinner ->                  
        Value = 0 
    ; 
        %max_count_row(Board, NumRC, Block, CountRowGood, NumRowGood),
        max_count_col(Board, NumRC, Block, CountColGood, NumColGood),
        max_count_row(Board, NumRC, OtherBlock, CountRowBad, NumRowBad),
        %max_count_col(Board, NumRC, OtherBlock, CountColBad, NumColBad),
        CountRowBad2 is CountRowBad // 2,
        TotalCount is CountColGood + CountRowBad2,
        Value is CountColGood / TotalCount
    ). 

value_next_move([Player, Board, Levels, OtherPlayer, MovesLeft], Value):-
    NumRC is 1,
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer; WhiteWinner = OtherPlayer, BlackWinner = Player),
    (Player = 'p1' -> Block = 'W', OtherBlock = 'B'; Block = 'B', OtherBlock = 'W'),
    %max_count_row(Board, NumRC, Block, CountRowGood, NumRowGood),
    max_count_col(Board, NumRC, Block, CountColGood, NumColGood),
    max_count_row(Board, NumRC, OtherBlock, CountRowBad, NumRowBad),
    %max_count_col(Board, NumRC, OtherBlock, CountColBad, NumColBad),
    CountRowBad2 is CountRowBad // 2,
    TotalCount is CountColGood + CountRowBad2,
    Value is CountColGood / TotalCount.  

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
    move2([Player, Board, Levels, OtherPlayer, MovesLeft], Move, OtherGameState),
    game_over(OtherGameState, Winner).

block_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], BlockerMove) :-
    (Player = 'p1' -> WhiteWinner = Player, BlackWinner = OtherPlayer ; WhiteWinner = OtherPlayer, BlackWinner = Player),
    findall([Value, [N2, X, Y]], (
        generate_coordinates(1, 4, N2),
        \+ N2 = N,
        move2([Player, Board, Levels, OtherPlayer, MovesLeft], [N2, X, Y], NewGameState),
        value_next_move(NewGameState, Value)
    ), MovesWithValues),
    (Player = WhiteWinner ->
        max_member([_, BlockerMove], MovesWithValues) 
    ;
        min_member([_, BlockerMove], MovesWithValues) 
    ).


choose_move([Player, Board, Levels, OtherPlayer, MovesLeft], 2, Move) :-
    write(MovesLeft),nl,
    valid_moves(Board, Levels, Moves),
    is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], Player, Moves, MoveWinner),
    (MoveWinner \= none ->
        Move = MoveWinner
    ;
        is_any_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], OtherPlayer, Moves, MoveWinnerOther),
        (MoveWinnerOther \= none -> 
            block_winning_move([Player, Board, Levels, OtherPlayer, MovesLeft], MoveWinnerOther, BlockerMove),
            Move = BlockerMove
        ;
            choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, BestMove),
            Move = BestMove
        )
    ).


choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], [Move], Move).

choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], [Move|Moves], BestMove) :-
    move2([Player, Board, Levels, OtherPlayer, MovesLeft], Move, NewGameState), 
    value_next_move(NewGameState, Value),
    choose_best_move([Player, Board, Levels, OtherPlayer, MovesLeft], Moves, OtherMove),
    move2([Player, Board, Levels, OtherPlayer, MovesLeft], OtherMove, OtherGameState),
    value_next_move(OtherGameState, OtherValue),
    (Value < OtherValue -> BestMove = Move; BestMove = OtherMove).   
    



move([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2], [N, Y, X], [OtherPlayer, Board8, Levels8, Player, Moves1, Color1, Color2]):-
    Moves1 is MovesLeft-1,
    NewX is 1+(X-1)*2, NewY is 10 - Y,

    % Get block
    piece_from_number(N, Piece),
    piece_coordinates(Piece, PieceConfig), 
    get_value(PieceConfig, 0, 0, V0),
    get_value(PieceConfig, 0, 1, V1),
    get_value(PieceConfig, 0, 2, V2),
    get_value(PieceConfig, 0, 3, V3),
    get_value(PieceConfig, 1, 0, V4),
    get_value(PieceConfig, 1, 1, V5),
    get_value(PieceConfig, 1, 2, V6),
    get_value(PieceConfig, 1, 3, V7),

    % Update board
    get_value(Board, NewY, NewX, Value), % Para que serve isto?
    update_piece(Board, NewY, NewX, V0, Board1),
    update_piece(Board1, NewY, NewX+1, V1, Board2),
    update_piece(Board2, NewY, NewX+2, V2, Board3),
    update_piece(Board3, NewY, NewX+3, V3, Board4),
    update_piece(Board4, NewY+1, NewX, V4, Board5),
    update_piece(Board5, NewY+1, NewX+1, V5, Board6),
    update_piece(Board6, NewY+1, NewX+2, V6, Board7),
    update_piece(Board7, NewY+1, NewX+3, V7, Board8),

    % Update levels
    get_value(Levels, NewY, NewX, Level), NewLevel is Level+1,
    update_piece(Levels, NewY, NewX, NewLevel, Levels1),
    update_piece(Levels1, NewY, NewX+1, NewLevel, Levels2),
    update_piece(Levels2, NewY, NewX+2, NewLevel, Levels3),
    update_piece(Levels3, NewY, NewX+3, NewLevel, Levels4),
    update_piece(Levels4, NewY+1, NewX, NewLevel, Levels5),
    update_piece(Levels5, NewY+1, NewX+1, NewLevel, Levels6),
    update_piece(Levels6, NewY+1, NewX+2, NewLevel, Levels7),
    update_piece(Levels7, NewY+1, NewX+3, NewLevel, Levels8).


move2([Player, Board, Levels, OtherPlayer, MovesLeft], [N, X, Y], [OtherPlayer, Board8, Levels8, Player, Moves1]):-
    Moves1 is MovesLeft-1,
    NewX is 1+(X-1)*2, NewY is 10 - Y,

    % Get block
    piece_from_number(N, Piece),
    piece_coordinates(Piece, PieceConfig), 
    get_value(PieceConfig, 0, 0, V0),
    get_value(PieceConfig, 0, 1, V1),
    get_value(PieceConfig, 0, 2, V2),
    get_value(PieceConfig, 0, 3, V3),
    get_value(PieceConfig, 1, 0, V4),
    get_value(PieceConfig, 1, 1, V5),
    get_value(PieceConfig, 1, 2, V6),
    get_value(PieceConfig, 1, 3, V7),

    % Update board
    get_value(Board, NewY, NewX, Value), % Para que serve isto?
    update_piece(Board, NewY, NewX, V0, Board1),
    update_piece(Board1, NewY, NewX+1, V1, Board2),
    update_piece(Board2, NewY, NewX+2, V2, Board3),
    update_piece(Board3, NewY, NewX+3, V3, Board4),
    update_piece(Board4, NewY+1, NewX, V4, Board5),
    update_piece(Board5, NewY+1, NewX+1, V5, Board6),
    update_piece(Board6, NewY+1, NewX+2, V6, Board7),
    update_piece(Board7, NewY+1, NewX+3, V7, Board8),

    % Update levels
    get_value(Levels, NewY, NewX, Level), NewLevel is Level+1,
    update_piece(Levels, NewY, NewX, NewLevel, Levels1),
    update_piece(Levels1, NewY, NewX+1, NewLevel, Levels2),
    update_piece(Levels2, NewY, NewX+2, NewLevel, Levels3),
    update_piece(Levels3, NewY, NewX+3, NewLevel, Levels4),
    update_piece(Levels4, NewY+1, NewX, NewLevel, Levels5),
    update_piece(Levels5, NewY+1, NewX+1, NewLevel, Levels6),
    update_piece(Levels6, NewY+1, NewX+2, NewLevel, Levels7),
    update_piece(Levels7, NewY+1, NewX+3, NewLevel, Levels8).