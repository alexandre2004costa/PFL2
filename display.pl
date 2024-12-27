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
    write(' '), word('1 -> Play', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    %word('2-> Instructions', Symbol, Size),
    %line(1, Symbol, Size),
    write(' '), word('2 -> Configurations', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('3 -> Exit', Symbol, Size),
    write(' '), line(1, Symbol, Size),
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl. 

print_banner(play):-
    Size is 60, Symbol = 0x2551,
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('Mode', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1 -> User vs User', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('2 -> User vs Pc', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('3 -> Pc vs Pc', Symbol, Size),
    write(' '), line(1, Symbol, Size),
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

print_banner(level, N):-
    Size is 60, Symbol = 0x2551, 
    (N = 0 -> Title = 'PC - AI Level'; N = 1 -> Title = 'Pc 1 - AI Level'; N = 2 -> Title = 'Pc 2 - AI Level'),
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word(Title, Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1 -> Level 1', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('2 -> Level 2', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

print_banner(colors, N):-
    Size is 60, Symbol = 0x2551,
    (N = 1 -> Title = 'Colors for Player 1', Letter = 'W'; N = 2 -> Title = 'Colors for Player 2', Letter = 'B'),
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word(Title, Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x2551, white), print_n(3, ' '),
        write_color('1 -> ', red), display_code(Letter, red), print_n(2, ' '),
        write_color('2 -> ', green), display_code(Letter, green), print_n(2, ' '),
        write_color('3 -> ',yellow ), display_code(Letter, yellow), print_n(2, ' '),
        write_color('4 -> ',blue ), display_code(Letter, blue), print_n(2, ' '), 
        write_color('5 -> ',magenta ), display_code(Letter, magenta), print_n(2, ' '),
        write_color('6 -> ',cyan ), display_code(Letter, cyan), print_n(2, ' '),
        write_color('7 -> ',white ), display_code(Letter, white), print_n(2, ' '),
        reset_color, print_n(1, ' '), put_code_color(0x2551, white), nl,
    write(' '), line(1, Symbol, Size),
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

print_banner(display_colors, Color1, Color2):-
    Size is 60, Symbol = 0x2551,
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('Colors', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x2551, white), print_n(11, ' '),
        write('Player 1 -> '), display_code('W', Color1), display_code('W', Color1), print_n(10, ' '),
        write('Player 2 -> '), display_code('B', Color2), display_code('B', Color2), print_n(11, ' '), put_code_color(0x2551, white), nl,
    write(' '), line(1, Symbol, Size),
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.


% Auxiliar
print_numbers(10):- !.
print_numbers(N):-
    write(N), write(' '), 
    N2 is N+1, print_numbers(N2).

level_color(Ratio, Row, Color1, Color2, Color1) :-
    RatioNorm is round(Ratio * 12), % Normalization 
    Row =< RatioNorm.

level_color(Ratio, Row, Color1, Color2, Color2) :-
    RatioNorm is round(Ratio * 12), % Normalization
    Row > RatioNorm.


% Board
display_board([], Color1, Color2, Ratio) :- !.
display_board([Row | Rest], Color1, Color2, Ratio) :-
    level_color(Ratio, 12, Color1, Color2, ColorRow1),
    level_color(Ratio, 1, Color1, Color2, ColorRow12),

    length(Row, Length), Len is Length + 6, RowsLen is 11, N is 1, 
    print_n(5,' '), put_code_color(0x250C, white), print_n_code(Len, 0x2500, white), put_code_color(0x2510, white),
    print_n(5,' '), put_code_color(0x250C, bold_cyan), print_n_code(2, 0x2500, bold_cyan), put_code_color(0x2510, bold_cyan), nl,

    print_n(5,' '), put_code_color(0x2502, white), write(' '), print_n_code(2, 0x2588,white), print_n_code(Length, 0x2593, Color1), print_n_code(2, 0x2588,white), write(' '), put_code_color(0x2502, white),
    print_n(5,' '), put_code_color(0x2502, bold_cyan), print_n_code(2, 0x2592, ColorRow1), put_code_color(0x2502, bold_cyan), nl,

    display_board_rows([Row | Rest], RowsLen, Color1, Color2, Ratio),

    print_n(5,' '), put_code_color(0x2502, white), write(' '), print_n_code(2, 0x2588,white), print_n_code(Length, 0x2593, Color1), print_n_code(2, 0x2588,white), write(' '), put_code_color(0x2502, white),
    print_n(5,' '), put_code_color(0x2502, bold_cyan), print_n_code(2, 0x2592, ColorRow12), put_code_color(0x2502, bold_cyan), nl,

    print_n(5,' '), put_code_color(0x2514, white), print_n_code(Len, 0x2500, white), put_code_color(0x2518, white),
    print_n(5,' '), put_code_color(0x2514, bold_cyan), print_n_code(2, 0x2500, bold_cyan), put_code_color(0x2518, bold_cyan), nl,
    print_n(11, ' '), print_numbers(N), nl.

display_board_rows([], _, Color1, Color2, Ratio) :- !.
display_board_rows([Row | Rest], RowsLen, Color1, Color2, Ratio) :-
    RowNumber is RowsLen-1,
    display_row(Row, RowNumber, Color1, Color2, Ratio),     % Exibe a linha atual
    display_board_rows(Rest, RowNumber, Color1, Color2, Ratio).  % Exibe as linhas restantes

display_row(Row, RowNumber, Color1, Color2, Ratio) :-
    RowNumber1 is RowNumber+1,
    level_color(Ratio, RowNumber1, Color1, Color2, ColorRow),
    RowNumber < 10,
    print_n(3,' '), write(RowNumber), write(' '), put_code_color(0x2502, white), write(' '), put_code_color(0x2592, Color2), put_code_color(0x2592, Color2),           % Começa com uma borda lateral
    display_cells(Row, Color1, Color2),   % Exibe as células da linha
    put_code_color(0x2592, Color2), put_code_color(0x2592, Color2), write(' '), put_code_color(0x2502, white),
    print_n(5,' '), put_code_color(0x2502, bold_cyan), put_code_color(0x2592, ColorRow), put_code_color(0x2592, ColorRow),           % Começa com uma borda lateral
    put_code_color(0x2502, bold_cyan), nl.       % Fecha com uma borda lateral e pula linha

display_row(Row, RowNumber, Color1, Color2, Ratio) :-
    RowNumber1 is RowNumber+1,
    level_color(Ratio, RowNumber1, Color1, Color2, ColorRow),
    print_n(5,' '), put_code_color(0x2502 , white), write(' '), put_code_color(0x2592 , Color2), put_code_color(0x2592, Color2),           % Começa com uma borda lateral
    display_cells(Row, Color1, Color2),   % Exibe as células da linha
    put_code_color(0x2592, Color2), put_code_color(0x2592, Color2), write(' '), put_code_color(0x2502, white),      % Fecha com uma borda lateral e pula linha
    print_n(5,' '), put_code_color(0x2502, bold_cyan), put_code_color(0x2592, ColorRow), put_code_color(0x2592, ColorRow),           % Começa com uma borda lateral
    put_code_color(0x2502, bold_cyan), nl.

display_cells([], Color1, Color2) :- !.   % Caso base: nada para exibir
display_cells([Cell | Rest], Color1, Color2) :-
    display_code(Cell, Color1, Color2),
    display_cells(Rest, Color1, Color2).     % Continua exibindo as células restantes

display_code('S',Color1, Color2):- put_code_color(0x2588, white).
display_code('W',Color1, Color2):- put_code_color(0x2593, Color1).
display_code('B',Color1, Color2):- put_code_color(0x2592, Color2).


displayPlayer('p1'):-
    write('Turn: Player 1'), nl.

displayPlayer('p2'):-
    write('Turn: Player 2'), nl.

displayPlayer('pc1'):-
    write('Turn: Pc 1'), nl.

displayPlayer('pc2'):-
    write('Turn: Pc 2'), nl.

display_pieces(Color1, Color2):-
    piece_coordinates(piece1, [P1, P12]),
    piece_coordinates(piece2, [P2, P22]),
    piece_coordinates(piece3, [P3, P32]),
    piece_coordinates(piece4, [P4, P42]), nl,
    write('Piece 1 -> '), display_cells(P1,Color1, Color2), print_n(3, ' '),
    write('Piece 2 -> '), display_cells(P2,Color1, Color2), print_n(3, ' '),
    write('Piece 3 -> '), display_cells(P3,Color1, Color2), print_n(3, ' '),
    write('Piece 4 -> '), display_cells(P4,Color1, Color2), nl,
    write('           '), display_cells(P12,Color1, Color2), print_n(3, ' '),
    write('           '), display_cells(P22,Color1, Color2), print_n(3, ' '),
    write('           '), display_cells(P32,Color1, Color2), print_n(3, ' '),
    write('           '), display_cells(P42,Color1, Color2), nl, nl.


display_game([Player, Board, Color1, Color2, Ratio]):-
    display_board(Board, Color1, Color2, Ratio), nl,
    displayPlayer(Player).