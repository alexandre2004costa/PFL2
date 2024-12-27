% Symbol               - Code
% Left Top Corner      - 0x250C
% Right Top Corner     - 0x2510
% Left Bottom Corner   - 0x2514
% Right Bottom Corner  - 0x2518
% Horizontal line      - 0x2500
% Vertical line        - 0x2502
 

% Printing
print_n(0,S):-!.
print_n(N,S):- N1 is N-1, print_n(N1, S), put_char(S).

print_n_code(0,S, Color):-!.
print_n_code(N, S, Color):- N1 is N-1, print_n_code(N1, S, Color), put_code_color(S, Color).

line(0,S,P):-!.
line(X,S,P):- X1 is X-1, put_code_color(S, white), print_n(P,' '), put_code_color(S, white), nl, line(X1,S,P).

word(Text, Symbol, Size) :- atom_length(Text, TextSize), 
    Padding is (Size - TextSize - 2) // 2, PaddingRight is Size - TextSize - Padding, 
    put_code_color(Symbol, white), print_n(Padding, ' '), write(Text), print_n(PaddingRight, ' '), put_code_color(Symbol, white), nl.


% Menu
print_title:- 
    % Line 1
    put_code_color(0x2551, white), print_n(9, ' '),
    print_n_code(6, 0x2588, white), print_n(3, ' '), print_n_code(2, 0x2588, white), print_n(7, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), 
    print_n_code(3, 0x2588, white), print_n(4, ' '), print_n_code(2, 0x2588, white), print_n(3, ' '), print_n_code(6, 0x2588, white), print_n(2, ' '), 
    print_n(9, ' '), put_code_color(0x2551, white), nl,

    % Line 2
    write(' '), put_code_color(0x2551, white), print_n(9, ' '),
    print_n_code(2, 0x2588, white), print_n(3, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), print_n_code(2, 0x2588, white), print_n(7, ' '), 
    print_n_code(2, 0x2588, white), print_n(2, ' '), print_n_code(4, 0x2588, white), print_n(3, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), 
    print_n_code(2, 0x2588, white), print_n(4, ' '), print_n_code(2, 0x2588, white), print_n(1, ' '),
    print_n(9, ' '), put_code_color(0x2551, white), nl,

    % Line 3
    write(' '), put_code_color(0x2551, white), print_n(9, ' '),
    print_n_code(6, 0x2588, white), print_n(3, ' '), print_n_code(2, 0x2588, white), print_n(7, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), 
    print_n_code(2, 0x2588, white), print_n(1, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), 
    print_n_code(2, 0x2588, white), print_n(4, ' '), print_n_code(2, 0x2588, white), print_n(1, ' '),
    print_n(9, ' '), put_code_color(0x2551, white), nl,

    % Line 4
    write(' '), put_code_color(0x2551, white), print_n(9, ' '),
    print_n_code(2, 0x2588, white), print_n(3, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), print_n_code(2, 0x2588, white), print_n(7, ' '), 
    print_n_code(2, 0x2588, white), print_n(2, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), print_n_code(2, 0x2588, white), print_n(1, ' '), 
    print_n_code(2, 0x2588, white), print_n(2, ' '), print_n_code(2, 0x2588, white), print_n(1, ' '), print_n_code(2, 0x2584, white), print_n(1, ' '), 
    print_n_code(2, 0x2588, white), print_n(1, ' '),
    print_n(9, ' '), put_code_color(0x2551, white), nl,

    % Line 5
    write(' '), put_code_color(0x2551, white), print_n(9, ' '),
    print_n_code(6, 0x2588, white), print_n(3, ' '), print_n_code(7, 0x2588, white), print_n(2, ' '), print_n_code(2, 0x2588, white), print_n(2, ' '), 
    print_n_code(2, 0x2588, white), print_n(3, ' '), print_n_code(4, 0x2588, white), print_n(3, ' '), print_n_code(6, 0x2588, white), print_n(2, ' '),
    print_n(9, ' '), put_code_color(0x2551, white), nl,

    % Line 6
    write(' '), put_code_color(0x2551, white), print_n(9, ' '),
    print_n(37, ' '), print_n_code(5, 0x2580, white),
    print_n(9, ' '), put_code_color(0x2551, white), nl.


print_banner(menu):-
    Size is 60, Symbol = 0x2551,
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), print_title,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1-> Play', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    %word('2-> Instructions', Symbol, Size),
    %line(1, Symbol, Size),
    write(' '), word('2-> Configurations', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('3-> Exit', Symbol, Size),
    write(' '), line(1, Symbol, Size),
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl. 

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

print_bannerColors(Size, Symbol):-
    print_n(Size, Symbol),  
    line(1, Symbol, Size),  
    write_color('1 -> ', red), display_code('B', red), display_code('W', red),nl,  
    write_color('2 -> ', green), display_code('B', green), display_code('W', green),nl, 
    write_color('3 -> ',yellow ), display_code('B', yellow), display_code('W', yellow),nl, 
    write_color('4 -> ',blue ), display_code('B', blue), display_code('W', blue),nl, 
    write_color('5 -> ',magenta ), display_code('B', magenta), display_code('W', magenta),nl, 
    write_color('6 -> ',cyan ), display_code('B', cyan), display_code('W', cyan),nl,
    write_color('7 -> ',white ), display_code('B', white), display_code('W', white),nl,
    write_color('8 -> ',bold_cyan ), display_code('B', bold_cyan), display_code('W', bold_cyan),nl,reset_color,
    print_n(Size, Symbol),nl.

print_numbers(10):- !.
print_numbers(N):-
    write(N), write(' '), 
    N2 is N+1, print_numbers(N2).

%level_color(1, Row, Color1, Color2, Color1).

level_color(Ratio, Row, Color1, Color2, Color1) :-
    RatioNorm is round(Ratio * 12), % Normalization 
    Row =< RatioNorm.

level_color(Ratio, Row, Color1, Color2, Color2) :-
    RatioNorm is round(Ratio * 12), % Normalization
    Row > RatioNorm.

display_board([], Color1, Color2, Ratio) :- !.
display_board([Row | Rest], Color1, Color2, Ratio) :-
    level_color(Ratio, 12, Color1, Color2, ColorRow1),
    level_color(Ratio, 1, Color1, Color2, ColorRow12),
    length(Row, Length), Len is Length + 6, RowsLen is 11, N is 1, 
    write(' '), put_code_color(0x250C, white), print_n_code(Len, 0x2500, white), put_code_color(0x2510, white),
    write(' '), put_code_color(0x250C, bold_cyan), print_n_code(2, 0x2500, bold_cyan), put_code_color(0x2510, bold_cyan), nl,

    write(' '), put_code_color(0x2502, white), write(' '), print_n_code(2, 0x2588,white), print_n_code(Length, 0x2593, Color1), print_n_code(2, 0x2588,white), write(' '), put_code_color(0x2502, white),
    write(' '), put_code_color(0x2502, bold_cyan), print_n_code(2, 0x2592, ColorRow1), put_code_color(0x2502, bold_cyan), nl,

    display_board_rows([Row | Rest], RowsLen, Color1, Color2, Ratio),

    write(' '), put_code_color(0x2502, white), write(' '), print_n_code(2, 0x2588,white), print_n_code(Length, 0x2593, Color1), print_n_code(2, 0x2588,white), write(' '), put_code_color(0x2502, white),
    write(' '), put_code_color(0x2502, bold_cyan), print_n_code(2, 0x2592, ColorRow12), put_code_color(0x2502, bold_cyan), nl,

    write(' '), put_code_color(0x2514, white), print_n_code(Len, 0x2500, white), put_code_color(0x2518, white),
    write(' '), put_code_color(0x2514, bold_cyan), print_n_code(2, 0x2500, bold_cyan), put_code_color(0x2518, bold_cyan), nl,
    print_n(6, ' '), print_numbers(N), nl.

display_board_rows([], _, Color1, Color2, Ratio) :- !.
display_board_rows([Row | Rest], RowsLen, Color1, Color2, Ratio) :-
    RowNumber is RowsLen-1,
    display_row(Row, RowNumber, Color1, Color2, Ratio),     % Exibe a linha atual
    display_board_rows(Rest, RowNumber, Color1, Color2, Ratio).  % Exibe as linhas restantes

display_row(Row, RowNumber, Color1, Color2, Ratio) :-
    RowNumber1 is RowNumber+1,
    level_color(Ratio, RowNumber1, Color1, Color2, ColorRow),
    RowNumber < 10,
    write(RowNumber), put_code_color(0x2502, white), write(' '), put_code_color(0x2592, Color2), put_code_color(0x2592, Color2),           % Começa com uma borda lateral
    display_cells(Row, Color1, Color2),   % Exibe as células da linha
    put_code_color(0x2592, Color2), put_code_color(0x2592, Color2), write(' '), put_code_color(0x2502, white),
    write(' '), put_code_color(0x2502, bold_cyan), put_code_color(0x2592, ColorRow), put_code_color(0x2592, ColorRow),           % Começa com uma borda lateral
    put_code_color(0x2502, bold_cyan), nl.       % Fecha com uma borda lateral e pula linha

display_row(Row, RowNumber, Color1, Color2, Ratio) :-
    RowNumber1 is RowNumber+1,
    level_color(Ratio, RowNumber1, Color1, Color2, ColorRow),
    write(' '), put_code_color(0x2502 , white), write(' '), put_code_color(0x2592 , Color2), put_code_color(0x2592, Color2),           % Começa com uma borda lateral
    display_cells(Row, Color1, Color2),   % Exibe as células da linha
    put_code_color(0x2592, Color2), put_code_color(0x2592, Color2), write(' '), put_code_color(0x2502, white),      % Fecha com uma borda lateral e pula linha
    write(' '), put_code_color(0x2502, bold_cyan), put_code_color(0x2592, ColorRow), put_code_color(0x2592, ColorRow),           % Começa com uma borda lateral
    put_code_color(0x2502, bold_cyan), nl.

display_cells([], Color1, Color2) :- !.   % Caso base: nada para exibir
display_cells([Cell | Rest], Color1, Color2) :-
    display_code(Cell, Color1, Color2),
    display_cells(Rest, Color1, Color2).     % Continua exibindo as células restantes

display_code('S',Color1, Color2):- put_code_color(0x2588, white).
display_code('W',Color1, Color2):- put_code_color(0x2593, Color1).
display_code('B',Color1, Color2):- put_code_color(0x2592, Color2).


displayPlayer('p1'):-
    write('Player 1 move : '), nl.

displayPlayer('p2'):-
    write('Player 2 move : '), nl.

displayPlayer('pc1'):-
    write('Pc 1 move : '), nl.

displayPlayer('pc2'):-
    write('Pc 2 move : '), nl.

display_pieces(Color1, Color2):-
    piece_coordinates(piece1, [P1, P12]),
    piece_coordinates(piece2, [P2, P22]),
    piece_coordinates(piece3, [P3, P32]),
    piece_coordinates(piece4, [P4, P42]),nl,
    write(' Piece 1 -> '), display_cells(P1,Color1, Color2), 
    write(' Piece 2 -> '), display_cells(P2,Color1, Color2), 
    write(' Piece 3 -> '), display_cells(P3,Color1, Color2), 
    write(' Piece 4 -> '), display_cells(P4,Color1, Color2),nl,
    write('            '), display_cells(P12,Color1, Color2),
    write('            '), display_cells(P22,Color1, Color2),
    write('            '), display_cells(P32,Color1, Color2),
    write('            '), display_cells(P42,Color1, Color2),nl.


display_game([Player, Board, Color1, Color2, Ratio]):-
    display_board(Board, Color1, Color2, Ratio), nl,
    displayPlayer(Player).