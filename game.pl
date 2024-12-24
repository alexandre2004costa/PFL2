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
    
initial_state([Player, OtherPlayer], [Player, Board, OtherPlayer]).

play :- state(initial).

play_game:-
    board(X),
    initial_state([p1, p2], [P1, X, P2]),
    play_turn([P1, X, P2]).

play_turn([Player, Board, OtherPlayer]) :-
    display_game([Player, Board]),  
    game_over([Player, Board], Winner),  % Verifica se o jogo acabou
    ( Winner \= none ->                  % Se houver um vencedor
        format("~w venceu o jogo!~n", [Winner])  % Anuncia o vencedor
    ;
        % Continua o jogo se não houver vencedor
        read_input(N, X, Y),
        piece_from_number(N, Piece),
        move([Player, Board, OtherPlayer], [Piece, Y, X], NewState),
        play_turn(NewState)
    ).

read_input(N, X, Y) :- % Falta adicionar limits
    write('Escolha o tipo de peça (1-4): '),
    read(N),
    write('Digite a coordenada Y: '),
    read(Y),
    write('Digite a coordenada X: '),
    read(X).    

move([Player, Board, OtherPlayer], [Piece, Y, X], [OtherPlayer, Board8, Player]):-
    NewX is 1+(X-1)*2,
    NewY is 10 - Y,
    piece_coordinates(Piece, PieceConfig), 
    get_value(PieceConfig, 0, 0, V0),
    get_value(PieceConfig, 0, 1, V1),
    get_value(PieceConfig, 0, 2, V2),
    get_value(PieceConfig, 0, 3, V3),
    get_value(PieceConfig, 1, 0, V4),
    get_value(PieceConfig, 1, 1, V5),
    get_value(PieceConfig, 1, 2, V6),
    get_value(PieceConfig, 1, 3, V7),

    % Atualiza as colunas da linha 0
    update_piece(Board, NewY, NewX, V0, Board1),
    update_piece(Board1, NewY, NewX+1, V1, Board2),
    update_piece(Board2, NewY, NewX+2, V2, Board3),
    update_piece(Board3, NewY, NewX+3, V3, Board4),

    % Atualiza as colunas da linha 1
    update_piece(Board4, NewY+1, NewX, V4, Board5),
    update_piece(Board5, NewY+1, NewX+1, V5, Board6),
    update_piece(Board6, NewY+1, NewX+2, V6, Board7),
    update_piece(Board7, NewY+1, NewX+3, V7, Board8).


