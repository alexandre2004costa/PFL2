print_n(0,S):-!.
print_n(N,S):- N1 is N-1, print_n(N1, S), put_char(S).

line(0,S,P):-!.
line(X,S,P):- X1 is X-1, put_char(S), print_n(P-2,' '), put_char(S), nl,  line(X1,S,P).

word(Text, Symbol, Size) :- atom_length(Text, TextSize), Padding is (Size - TextSize - 2) // 2, 
    PaddingRight is Size - TextSize - 2 - Padding, 
    put_char(Symbol), print_n(Padding, ' '), write(Text), print_n(PaddingRight, ' '), put_char(Symbol), nl.

print_banner(Size, Symbol):-
    print_n(Size, Symbol), nl,  
    line(1, Symbol, Size),  
    word('Blinq', Symbol, Size),
    line(2, Symbol, Size),  
    word('1-> Play', Symbol, Size),
    line(1, Symbol, Size), 
    word('2-> Leave', Symbol, Size),
    print_n(Size, Symbol), nl.  

print_bannerPlay(Size, Symbol):-
    print_n(Size, Symbol), nl,  
    line(1, Symbol, Size),  
    word('Blinq', Symbol, Size),
    line(2, Symbol, Size),  
    word('1-> user vs user', Symbol, Size),
    line(1, Symbol, Size), 
    word('2-> user vs pc', Symbol, Size),
    line(1, Symbol, Size), 
    word('3-> pc vs pc', Symbol, Size),
    line(1, Symbol, Size), 
    word('4-> back', Symbol, Size),
    line(1, Symbol, Size), 
    print_n(Size, Symbol), nl. 



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

board([
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', 'W', '0', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', '0', 'W', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).

% Exibe o tabuleiro linha por linha
display_board([]) :- !. % Caso base: nada para exibir
display_board([Row | Rest]) :-
    display_row(Row),     % Exibe a linha atual
    display_board(Rest).  % Exibe as linhas restantes

% Exibe uma linha do tabuleiro
display_row(Row) :-
    write('|'),           % Começa com uma borda lateral
    display_cells(Row),   % Exibe as células da linha
    write('|'), nl.       % Fecha com uma borda lateral e pula linha

% Exibe cada célula de uma linha
display_cells([]) :- !.   % Caso base: nada para exibir
display_cells([Cell | Rest]) :-
    put_char(Cell),
    display_cells(Rest).     % Continua exibindo as células restantes

displayPlayer(p1):-
    write('Player 1 move : '), nl.

displayPlayer(p2):-
    write('Player 2 move : '), nl.

displayPlayer(pc1):-
    write('Pc 1 move : '), nl.

displayPlayer(pc2):-
    write('Pc 2 move : '), nl.


get_value([Row|_], 0, X, Value):- 
    get_value_in_row(Row, X, Value).

get_value([_|Rest], Y, X, Value):- 
    Y > 0,
    Y1 is Y - 1,
    get_value(Rest, Y1, X, Value).

% Acessa o valor na coluna X de uma linha
get_value_in_row([Value|_], 0, Value).
get_value_in_row([_|Rest], X, Value):- 
    X > 0,
    X1 is X - 1,
    get_value_in_row(Rest, X1, Value).

piece_from_number(1, piece1).
piece_from_number(2, piece2).
piece_from_number(3, piece3).
piece_from_number(4, piece4).

piece_coordinates(piece1, [['W','W'], ['0','0']]).
piece_coordinates(piece2, [['W','0'], ['w','0']]).
piece_coordinates(piece3, [['0','0'], ['w','w']]).
piece_coordinates(piece4, [['0','W'], ['0','w']]).


update_piece([Row|Rest], 1, Col, Piece, [NewRow|Rest]):- %Row of alteration
    update_piece_col(Row, Col, Piece, NewRow).

update_piece([Row|Rest], RowIndex, Col, Piece, [Row|NewRest]):- %Row of alteration
    RowIndex > 1,
    Row1 is RowIndex-1,
    update_piece(Rest, Row1, Col, Piece, NewRest).

update_piece_col([_|Rest], 1, Piece, [Piece|Rest]). %Column of alteration

update_piece_col([Char|Rest], Col, Piece, [Char|Result]):- %Column of alteration
    Col > 1,
    Col1 is Col-1,
    update_piece_col(Rest, Col1, Piece, Result).
    
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
initial_state([Player, OtherPlayer], [Player, Board, OtherPlayer]).


display_game([Player, Board]):-
    display_board(Board), nl,
    displayPlayer(Player).

play :- state(initial).

play_game:-
    board(X),
    initial_state([p1, p2], [P1, X, P2]),
    play_turn([P1, X, P2]).

play_turn([Player, Board, OtherPlayer]) :-
    display_game([Player, Board]),  
    read_input(N,X,Y),
    piece_from_number(N, Piece),
    move([Player, Board, OtherPlayer], [Piece, Y, X], NewState),
    play_turn(NewState).

read_input(N, X, Y) :- % Falta adicionar limits
    write('Escolha o tipo de peça (1-4): '),
    read(N),
    write('Digite a coordenada Y: '),
    read(Y),
    write('Digite a coordenada X: '),
    read(X).    

move([Player, Board, OtherPlayer], [Piece, Y, X], [OtherPlayer, Board4, Player]):-
    NewY is 10 - Y, 
    piece_coordinates(Piece, PieceConfig),
    get_value(PieceConfig, 0, 0,V0),
    get_value(PieceConfig, 0, 1,V1),
    get_value(PieceConfig, 1, 0,V2),
    get_value(PieceConfig, 1, 1,V3),
    update_piece(Board, NewY, X,V0, Board1),
    update_piece(Board1, NewY, X+1,V1, Board2),
    update_piece(Board2, NewY+1, X,V2, Board3),
    update_piece(Board3, NewY+1, X+1,V3, Board4).

