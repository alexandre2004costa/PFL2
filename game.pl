:- consult(gameOver). 
:- consult(board). 
:- consult(display). 

% Menu
state(initial) :-
    print_banner(30, '0'),  
    write('Option: '), nl,
    read(Input),
    transition(initial, Input, NextState), 
    state(NextState). 

state(mode) :-
    print_bannerPlay(30, '0'),  
    write('Option: '), nl,
    read(Input),
    transition(mode, Input, NextState), 
    state(NextState). 

state(play_uu) :-
    play_game.

state(play_uc) :-
    write('User vs PC selected.'), nl.

state(play_cc) :-
    write('PC vs PC selected.'), nl.

state(exit) :-
    write('Exiting...'), nl. 

transition(initial, 1, mode).  
transition(initial, 2, exit).  
transition(mode, 1, play_uu).  
transition(mode, 2, play_uc).  
transition(mode, 3, play_cc).
transition(mode, 4, initial).
transition(_, _, initial). 


last([X], X).
last([Elem|Rest], X): last(Rest, X).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Isto parece-me estranho, parece que não está a fazer nada
initial_state([Player, OtherPlayer], [Player, Board, Levels, OtherPlayer]).

play :- state(initial).

play_game:-
    board(X), levels(Y),
    initial_state([p1, p2], [P1, X, Y, P2]),
    play_turn([P1, X, Y, P2]).

play_turn([Player, Board, Levels, OtherPlayer]) :-
    display_game([Player, Board]),  
    game_over([Player, Board], Winner),  
    ( Winner = 'T' ->                  
        write('Game tied!')  
    ;   
        Winner \= none ->                  
        format("~w venceu o jogo!~n", [Winner])  
    ;
        % Continue the game in case of no winner
        read_input(N,X,Y, Levels),
        piece_from_number(N, Piece),
        move([Player, Board, Levels, OtherPlayer], [Piece, Y, X], NewState),
        play_turn(NewState)
    ).

read_input(N, X, Y, Levels) :-
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
    validate_coordinates(X, Y). 

validate_input_coordinates(InputX, InputY, X, Y, Levels) :-
    (InputX < 1; InputX > 9; InputY < 1; InputY > 9),
    write('The coordinates must be between 1 and 9.'), nl, nl,
    validate_coordinates(X, Y). 


validate_input_coordinates(InputX, InputY, X, Y, Levels) :-
    validate_coordinates(InputX, InputY, Levels, Valid),
    ( Valid =:= 1 ->
        X = InputX, Y = InputY, !
    ; 
        Valid =:= 0 ->
        write('Invalid coordinates. Try again.'), nl,
        read_input_coordinates(X, Y, Levels)
    ).


validate_coordinates(X, Y, Levels, Valid) :-
    BoardX is 1+(X-1)*2, BoardY is 10 - Y, 
    BoardX2 is BoardX+2, BoardY2 is BoardY-1,

    1 is X mod 2, 1 is BoardY mod 2, 
    get_value(Levels, BoardY, BoardX, Level_LD), write( Level_LD), nl, Level_LD is 0,
    get_value(Levels, BoardY, BoardX2, Level_LT), write( Level_LT), nl, Level_LT is 0,
    get_value(Levels, BoardY2, BoardX, Level_RD), write( Level_RD), nl, Level_RD is 0,
    get_value(Levels, BoardY2, BoardX2, Level_RT), write( Level_RT), nl, Level_RT is 0,
    write('Level 0.'), nl, nl, Valid is 1.

validate_coordinates(X, Y, Levels, Valid) :-
    BoardX is 1+(X-1)*2, BoardY is 10 - Y,
    BoardX2 is BoardX+2, BoardY2 is BoardY-1,

    get_value(Levels, BoardY, BoardX, L1),
    get_value(Levels, BoardY, BoardX2, L2),
    get_value(Levels, BoardY2, BoardX, L3),
    get_value(Levels, BoardY2, BoardX2, L4),
    L2 is L1, L3 is L1, L4 is L1,

    ((0 is L1 mod 2, 1 is X mod 2, 1 is Y mod 2) ; (1 is L1 mod 2, 0 is X mod 2, 0 is Y mod 2)),

    write('Above levels.'), nl, nl, Valid is 1.

validate_coordinates(X, Y, Levels, Valid) :-
    Valid is 0.


move([Player, Board, Levels, OtherPlayer], [Piece, Y, X], [OtherPlayer, Board8, Levels8, Player]):-
    NewX is 1+(X-1)*2, NewY is 10 - Y,

    % Update board
    piece_coordinates(Piece, PieceConfig), 
    get_value(PieceConfig, 0, 0, V0),
    get_value(PieceConfig, 0, 1, V1),
    get_value(PieceConfig, 0, 2, V2),
    get_value(PieceConfig, 0, 3, V3),
    get_value(PieceConfig, 1, 0, V4),
    get_value(PieceConfig, 1, 1, V5),
    get_value(PieceConfig, 1, 2, V6),
    get_value(PieceConfig, 1, 3, V7),

    get_value(Board, NewY, NewX, Value),
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

