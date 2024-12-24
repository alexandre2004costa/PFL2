%Printing
print_n(0,S):-!.
print_n(N,S):- N1 is N-1, print_n(N1, S), put_char(S).

print_n_code(0,S):-!.
print_n_code(N,S):- N1 is N-1, print_n_code(N1, S), put_code(S).

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
    %word('2-> Instructions', Symbol, Size),
    %line(1, Symbol, Size),
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

print_numbers(10):- !.
print_numbers(N):-
    write(N), write(' '), 
    N2 is N+1, print_numbers(N2).


display_board([]) :- !.
display_board([Row | Rest]) :-
    length(Row, Length), Len is Length + 6, RowsLen is 11, N is 1, 
    write(' '), put_code(0x250C), print_n_code(Len, 0x2500), put_code(0x2510), nl,
    write(' '), put_code(0x2502), write(' '), print_n_code(2, 0x2588), print_n_code(Length, 0x2593), print_n_code(2, 0x2588), write(' '), put_code(0x2502), nl,
    display_board_rows([Row | Rest], RowsLen),
    write(' '), put_code(0x2502), write(' '), print_n_code(2, 0x2588), print_n_code(Length, 0x2593), print_n_code(2, 0x2588), write(' '), put_code(0x2502), nl,
    write(' '), put_code(0x2514), print_n_code(Len, 0x2500), put_code(0x2518), nl,
    print_n(6, ' '), print_numbers(N), nl.

display_board_rows([], _) :- !.
display_board_rows([Row | Rest], RowsLen) :-
    RowNumber is RowsLen-1,
    display_row(Row, RowNumber),     % Exibe a linha atual
    display_board_rows(Rest, RowNumber).  % Exibe as linhas restantes

display_row(Row, RowNumber) :-
    RowNumber < 10,
    write(RowNumber), put_code(0x2502), write(' '), put_code(0x2592), put_code(0x2592),           % Começa com uma borda lateral
    display_cells(Row),   % Exibe as células da linha
    put_code(0x2592), put_code(0x2592), write(' '), put_code(0x2502), nl.       % Fecha com uma borda lateral e pula linha

display_row(Row, RowNumber) :-
    write(' '), put_code(0x2502), write(' '), put_code(0x2592), put_code(0x2592),           % Começa com uma borda lateral
    display_cells(Row),   % Exibe as células da linha
    put_code(0x2592), put_code(0x2592), write(' '), put_code(0x2502), nl.       % Fecha com uma borda lateral e pula linha

display_cells([]) :- !.   % Caso base: nada para exibir
display_cells([Cell | Rest]) :-
    display_code(Cell),
    display_cells(Rest).     % Continua exibindo as células restantes

display_code('S'):- put_code(0x2588).
display_code('W'):- put_code(0x2593).
display_code('B'):- put_code(0x2592).


displayPlayer(p1):-
    write('Player 1 move : '), nl.

displayPlayer(p2):-
    write('Player 2 move : '), nl.

displayPlayer(pc1):-
    write('Pc 1 move : '), nl.

displayPlayer(pc2):-
    write('Pc 2 move : '), nl.

display_game([Player, Board]):-
    display_board(Board), nl,
    displayPlayer(Player).