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

play :- state(initial).

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
    write('User vs User selected.'), nl.

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

piece1([['W','W'],['0','0']]).
piece2([['W','0'],['w','0']]).
piece3([['0','0'],['w','w']]).
piece4([['0','W'],['0','w']]).

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


move([Player, Board, OtherPlayer], [piece1, Y, X], [OtherPlayer, Board4, Player]):-
    NewY is 10 - Y,
    update_piece(Board, NewY, X, 'w', Board1),
    update_piece(Board1, NewY, X+1, 'w', Board2),
    update_piece(Board2, NewY+1, X, '0', Board3),
    update_piece(Board3, NewY+1, X+1, '0', Board4).

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

play_game:-
    board(X),
    initial_state([p1, p2], [P1, X, P2]),
    play_turn([P1, X, P2]).

play_turn([Player, Board, OtherPlayer]) :-
    display_game([Player, Board]),       
    move([Player, Board, OtherPlayer], [piece1, 1, 1], NewState),
    play_turn(NewState).
    

