% Printing and auxiliar predicates ------------------------------------------------------------------------------------------------
% print_n(+Number, +Char)
% Prints a character a specified number of times
print_n(0,_):-!.
print_n(N,S):- N1 is N-1, print_n(N1, S), put_char(S).

% print_n_code(+Number, +Code, +Color)
% Prints a character of a given code a number of times with a specific color
print_n_code(0, _, _):-!.
print_n_code(N, S, Color):- N1 is N-1, print_n_code(N1, S, Color), put_code_color(S, Color).

% line(+Number, +Symbol, +Padding)
% Prints a line with a symbol at the borders and specified padding of spaces in between.
line(0, _, _):-!.
line(X,S,P):- X1 is X-1, put_code_color(S, white), print_n(P,' '), put_code_color(S, white), nl, line(X1,S,P).

% word(+Text, +Symbol, +Size)
% Prints a word centered with padding and bordered by a symbol
word(Text, Symbol, Size) :- atom_length(Text, TextSize), 
    Padding is (Size - TextSize - 2) // 2, PaddingRight is Size - TextSize - Padding, 
    put_code_color(Symbol, white), print_n(Padding, ' '), write(Text), print_n(PaddingRight, ' '), put_code_color(Symbol, white), nl.

% word_color(+Text, +Color, +Symbol, +Size)
% Prints a word centered with padding, with the text in a specific color and bordered by a symbol.
word_color(Text, Color, Symbol, Size) :- atom_length(Text, TextSize), 
    Padding is (Size - TextSize - 2) // 2, PaddingRight is Size - TextSize - Padding, 
    put_code_color(Symbol, white), print_n(Padding, ' '), write_color(Text, Color), print_n(PaddingRight, ' '), put_code_color(Symbol, white), nl.

% word_color_2(+Text1, +Color1, +Text2, +Color2, +Symbol, +Size)
% Prints two words centered with padding, each in a specific color, and bordered by a symbol.
word_color_2(Text, Color, Text2, Color2, Symbol, Size) :- atom_length(Text, TextSize), atom_length(Text2, TextSize2), 
    Padding is (Size - TextSize - TextSize2 - 2) // 2, PaddingRight is Size - TextSize - TextSize2 - Padding, 
    put_code_color(Symbol, white), print_n(Padding, ' '), write_color(Text, Color),  write_color(Text2, Color2), print_n(PaddingRight, ' '), put_code_color(Symbol, white), nl.

% print_numbers(+Number)
% Prints numbers from given number to 10.
print_numbers(10):- !.
print_numbers(N):-
    write(N), write(' '), 
    N2 is N+1, print_numbers(N2).

% level_color(+Ratio, +Row, -PlayerNumber)
% Assigns the player (1 or 2) based on the normalized ratio and row number.
level_color(Ratio, Row, 1) :-
    RatioNorm is round(Ratio * 12), % Normalization 
    Row =< RatioNorm.
level_color(Ratio, Row, 2) :-
    RatioNorm is round(Ratio * 12), % Normalization
    Row > RatioNorm.

% Banners -------------------------------------------------------------------------------------------------------------------------
% print_title(+Size, +Symbol)
% Prints a personalized title, inside a banner with a specific size and bordered with a chosen symbol.
print_title(Size, Symbol) :- 
    atom_codes(Atom1, [124,32,39,47,39,96,92,32,92,124,32,124,32,91,32,32,124,32,91,32,96,46,45,46,32,124,47,32,47,39,96,92,39,32,93]),
    atom_concat('|  \\__/ || |  | |  | | | || ', '\\__/ |  ', Line1),
    atom_codes(Atom2, [91,95,95,59,46,95,95,46,39,91,95,95,95,93,91,95,95,95,93,91,95,95,95,124,124,95,95,93,92,95,95,46,59,32,124,32,32]),

    write(' '), word_color(' __       __    _                    ', white, Symbol, Size),
    write(' '), word_color('[  |     [  |  (_)                   ', white, Symbol, Size),
    write(' '), word_color(' | |.--.  | |  __   _ .--.   .--. _  ', white, Symbol, Size),
    write(' '), word_color(Atom1, white, Symbol, Size),
    write(' '), word_color(Line1, white, Symbol, Size),
    write(' '), word_color(Atom2, white, Symbol, Size),
    write(' '), word_color('                                |__] ', white, Symbol, Size).


print_banner_menu(Color1, Color2):-
    Size is 60, Symbol = 0x2551,
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    print_title(Size, Symbol),
    write(' '), line(1, Symbol, Size), 
    write(' '), line(1, Symbol, Size), 
    write(' '), word_color_2('1 -> ', Color1, 'Play', Color2, Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    %word('2-> Instructions', Symbol, Size),
    %line(1, Symbol, Size),
    write(' '), word_color_2('2 -> ', Color1, 'Configurations', Color2, Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word_color_2('3 -> ', Color1, 'Exit', Color2, Symbol, Size),
    write(' '), line(1, Symbol, Size),
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl. 

% print_banner_play/0
% Displays a banner menu for selecting the game mode.
print_banner_play:-
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

% print_banner_level/0
% Displays a banner menu to choose the AI difficulty level for the computer opponent.
print_banner_level:-
    Size is 60, Symbol = 0x2551, 
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('PC - AI Level', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1 -> Level 1', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('2 -> Level 2', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

% print_banner_starter/0
% Displays a banner menu to select whether the player wants to play first or not.
print_banner_starter:-
    Size is 60, Symbol = 0x2551, 
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('Do you want to be the first to play?', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1 -> Yes', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('2 -> No ', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

% print_banner_pc/0
% Displays a banner menu to choose the type of computer vs computer game.
print_banner_pc:-
    Size is 60, Symbol = 0x2551, 
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('What type of battle you want to see?', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1 -> Pc AI 1 Vs Pc AI 1', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('2 -> Pc AI 1 Vs Pc AI 2', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('3 -> Pc AI 2 Vs Pc AI 1', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('4 -> Pc AI 2 Vs Pc AI 2', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

% print_banner_config/0
% Displays a banner to choose wich config we want to change
print_banner_config:-
    Size is 60, Symbol = 0x2551, 
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('What do you want to change?', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1 -> Colors', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('2 -> Board size', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('3 -> Board style', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

% print_banner_colors(+PlayerNumber)
% Displays a banner to show the possible colors to choose for player 1 or 2
print_banner_colors(N):-
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

% print_banner_board_size/0
% Displays a banner to choose wich board size we want
print_banner_board_size:-
    Size is 60, Symbol = 0x2551, 
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('What board size do you want?', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1 -> 5 x 5', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('2 -> 4 x 4', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

% print_banner_board_style/0
% Displays a banner to choose wich board style we want
print_banner_board_style:-
    Size is 60, Symbol = 0x2551, 
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size), 
    write(' '), word('What board style do you want?', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('1 -> Tradicional', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('2 -> Full level 1', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), word('3 -> Recent option', Symbol, Size),
    write(' '), line(1, Symbol, Size), 
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.

% print_banner_display_colors(+ColorPLayer1, +ColorPlayer2)
% Displays a banner to show the colors choosen for player 1 and player 2
print_banner_display_colors(Color1, Color2):-
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

% print_banner_final(+Winner, +ColorPlayer1, +ColorPlayer2)
% Displays a banner announcing the winner and options to play again or exit.
print_banner_final(Winner, Color1, Color2):-
    (Winner = 'p1' -> Text = 'Player 1', Color = Color1; Winner = 'p2' -> Text = 'Player 2', Color = Color2),
  
    Size is 60, Symbol = 0x2551,
    write(' '), put_code_color(0x2554, white), print_n_code(Size, 0x2550, white), put_code_color(0x2557, white), nl,
    write(' '), line(1, Symbol, Size),
    write(' '), word_color_2('Winner: ', white, Text, Color, Symbol, Size),
    write(' '), line(1, Symbol, Size), write(' '), line(1, Symbol, Size),
    write(' '), word('1 -> Play again               2 -> Exit ', Symbol, Size),
    write(' '), line(1, Symbol, Size),
    write(' '), put_code_color(0x255A, white), print_n_code(Size, 0x2550, white), put_code_color(0x255D, white), nl.


% Board and Game ------------------------------------------------------------------------------------------------------------------
% display_board(+Board, +Levels, +Color1, +Color2, +Ratio)
% Displays the board, a bar indicating the performance of each player in the game and the heigh levels of the blocks on the board 
display_board([], _, _, _, _) :- !.
display_board([Row | Rest], Levels, Color1, Color2, Ratio) :-
    level_color(Ratio, 12, N1), (N1 = 1 -> ColorRow1 = Color1, Code1 = 0x2593; N1 = 2 -> ColorRow1 = Color2, Code1 = 0x2592),
    level_color(Ratio, 1, N12), (N12 = 1 -> ColorRow12 = Color1, Code12 = 0x2593; N12 = 2 -> ColorRow12 = Color2, Code12 = 0x2592),

    length(Row, L), Length is L*2, Len is Length + 6, RowsLen is 11, N is 1, 
    print_n(5,' '), put_code_color(0x250C, white), print_n_code(Len, 0x2500, white), put_code_color(0x2510, white),
    print_n(5,' '), put_code_color(0x250C, bold_cyan), print_n_code(2, 0x2500, bold_cyan), put_code_color(0x2510, bold_cyan), 
    print_n(5,' '), put_code_color(0x250C, white), print_n_code(Len, 0x2500, white), put_code_color(0x2510, white), nl,

    print_n(5,' '), put_code_color(0x2502, white), write(' '), print_n_code(2, 0x2588, white), print_n_code(Length, 0x2593, Color1), print_n_code(2, 0x2588,white), write(' '), put_code_color(0x2502, white),
    print_n(5,' '), put_code_color(0x2502, bold_cyan), print_n_code(2, Code1, ColorRow1), put_code_color(0x2502, bold_cyan), 
    print_n(5,' '), put_code_color(0x2502, white), write(' '), print_n_code(2, 0x2588, white), print_n_code(Length, 0x2593, Color1), print_n_code(2, 0x2588,white), write(' '), put_code_color(0x2502, white), nl,

    display_board_rows([Row | Rest], Levels, RowsLen, Color1, Color2, Ratio),

    print_n(5,' '), put_code_color(0x2502, white), write(' '), print_n_code(2, 0x2588,white), print_n_code(Length, 0x2593, Color1), print_n_code(2, 0x2588,white), write(' '), put_code_color(0x2502, white),
    print_n(5,' '), put_code_color(0x2502, bold_cyan), print_n_code(2, Code12, ColorRow12), put_code_color(0x2502, bold_cyan), 
    print_n(5,' '), put_code_color(0x2502, white), write(' '), print_n_code(2, 0x2588,white), print_n_code(Length, 0x2593, Color1), print_n_code(2, 0x2588,white), write(' '), put_code_color(0x2502, white), nl,

    print_n(5,' '), put_code_color(0x2514, white), print_n_code(Len, 0x2500, white), put_code_color(0x2518, white),
    print_n(5,' '), put_code_color(0x2514, bold_cyan), print_n_code(2, 0x2500, bold_cyan), put_code_color(0x2518, bold_cyan), 
    print_n(5,' '), put_code_color(0x2514, white), print_n_code(Len, 0x2500, white), put_code_color(0x2518, white), nl,
    
    print_n(10, ' '), print_numbers(N), print_n(24, ' '), print_numbers(N), nl.

% display_board_rows(+Rows, +Levels, +RowsLen, +Color1, +Color2, +Ratio)
% Helper predicate to iterate through the rows of the board, the bar and the levels.
display_board_rows([], _, _, _, _, _) :- !.
display_board_rows([Row | Rest], [RowLevel | RestLevel], RowsLen, Color1, Color2, Ratio) :-
    RowNumber is RowsLen-1,
    display_row(Row, RowLevel, RowNumber, Color1, Color2, Ratio),
    display_board_rows(Rest, RestLevel, RowNumber, Color1, Color2, Ratio).

display_row(Row, RowLevel, RowNumber, Color1, Color2, Ratio) :-
    RowNumber1 is RowNumber+1, RowNumber < 10,
    level_color(Ratio, RowNumber1, N), (N = 1 -> ColorRow = Color1, Code = 0x2593; N = 2 -> ColorRow = Color2, Code = 0x2592),

    % Board 
    print_n(3,' '), write(RowNumber), write(' '), put_code_color(0x2502, white), write(' '), put_code_color(0x2592, Color2), put_code_color(0x2592, Color2),
    display_cells(Row, Color1, Color2),   % Display row cells of board
    put_code_color(0x2592, Color2), put_code_color(0x2592, Color2), write(' '), put_code_color(0x2502, white),

    % Elo Bar
    print_n(5,' '), put_code_color(0x2502, bold_cyan), put_code_color(Code, ColorRow), put_code_color(Code, ColorRow), put_code_color(0x2502, bold_cyan), 
    
    % Levels
    print_n(3,' '), write(RowNumber), write(' '), put_code_color(0x2502, white), write(' '), put_code_color(0x2592, Color2), put_code_color(0x2592, Color2),
    display_levels(RowLevel),
    put_code_color(0x2592, Color2), put_code_color(0x2592, Color2), write(' '), put_code_color(0x2502, white),
    nl.

display_row(Row, RowLevel, RowNumber, Color1, Color2, Ratio) :-
    RowNumber1 is RowNumber+1,
    level_color(Ratio, RowNumber1, N), (N = 1 -> ColorRow = Color1, Code = 0x2593; N = 2 -> ColorRow = Color2, Code = 0x2592),

    % Board
    print_n(5,' '), put_code_color(0x2502 , white), write(' '), put_code_color(0x2592 , Color2), put_code_color(0x2592, Color2),
    display_cells(Row, Color1, Color2),
    put_code_color(0x2592, Color2), put_code_color(0x2592, Color2), write(' '), put_code_color(0x2502, white),

    % Elo Bar 
    print_n(5,' '), put_code_color(0x2502, bold_cyan), put_code_color(Code, ColorRow), put_code_color(Code, ColorRow), put_code_color(0x2502, bold_cyan), 

    % Levels
    print_n(5,' '), put_code_color(0x2502 , white), write(' '), put_code_color(0x2592 , Color2), put_code_color(0x2592, Color2),
    display_levels(RowLevel),
    put_code_color(0x2592, Color2), put_code_color(0x2592, Color2), write(' '), put_code_color(0x2502, white),
    nl.

% display_cells(+Cells, +Color1, +Color2)
% Displays each cell in the row
display_cells([], _, _) :- !.
display_cells([Cell | Rest], Color1, Color2) :-
    display_code(Cell, Color1, Color2),
    display_code(Cell, Color1, Color2),
    display_cells(Rest, Color1, Color2).

% display_code(+Cell, +Color1, +Color2)
% Displays a specific cell in the board, based on the code and the color.
display_code('S', _, _):- put_code_color(0x2588, white).
display_code('W',Color1, _):- put_code_color(0x2593, Color1).
display_code('B', _, Color2):- put_code_color(0x2592, Color2).

% display_levels(+Levels)
% Displays the level of each cell in the row
display_levels([]) :- !.
display_levels([Cell | Rest]) :- write(Cell), write(Cell), display_levels(Rest).


% display_player_moves(+Player, +Moves, +Color)
% Displays the current player and the number of moves left.
 display_player_moves(Player, Moves, Color):-
    (Player = 'p1' -> Text = 'Player 1', Code = 0x2593; Player = 'p2' -> Text = 'Player 2', Code = 0x2592),
    atom_length(Text, TextSize), (Moves < 10 -> MoveSize = 1; MoveSize = 2), 
    Size is 73-42-TextSize-MoveSize,

    write(' '), put_code_color(0x2551, Color), write(' '), write(' '),
    write('Turn: '), write_color(Text, Color), write(' '), put_code_color(Code, Color), put_code_color(Code, Color),
    print_n(19, ' '), write('Moves left: '), write(Moves),
    print_n(Size, ' '), put_code_color(0x2551, Color), nl.

% display_pieces(+Color1, +Color2, +Color)
% Displays the possible blocks pieces players can choose to use
display_pieces(Color1, Color2, Color):-
    piece_coordinates(piece1, [P1, P12]),
    piece_coordinates(piece2, [P2, P22]),
    piece_coordinates(piece3, [P3, P32]),
    piece_coordinates(piece4, [P4, P42]),

    write(' '), put_code_color(0x2551, Color),  write(' '), write(' '), 
    write('Piece 1 -> '), display_cells(P1,Color1, Color2), print_n(3, ' '),
    write('Piece 2 -> '), display_cells(P2,Color1, Color2), print_n(3, ' '),
    write('Piece 3 -> '), display_cells(P3,Color1, Color2), print_n(3, ' '),
    write('Piece 4 -> '), display_cells(P4,Color1, Color2), 
    write(' '), write(' '), put_code_color(0x2551, Color), nl,

    write(' '), put_code_color(0x2551, Color),  write(' '), write(' '), 
    write('           '), display_cells(P12,Color1, Color2), print_n(3, ' '),
    write('           '), display_cells(P22,Color1, Color2), print_n(3, ' '),
    write('           '), display_cells(P32,Color1, Color2), print_n(3, ' '),
    write('           '), display_cells(P42,Color1, Color2), 
    write(' '), write(' '), put_code_color(0x2551, Color), nl.


% display_game(+GameState)
% Displays the game, including the board and other information about the game state.

display_game([Player, Board, Levels, OtherPlayer, MovesLeft, Color1, Color2, BoardSize, BoardStyle, Ratio]):-
    display_board(Board, Levels, Color1, Color2, Ratio), nl,

    Size is 73, (Player = 'p1' -> Color = Color1; Player = 'p2' -> Color = Color2),
    write(' '), put_code_color(0x2554, Color), print_n_code(Size, 0x2550, Color), put_code_color(0x2557, Color), nl,
    display_player_moves(Player, MovesLeft, Color),
    write(' '), put_code_color(0x2551, Color), print_n(Size, ' '), put_code_color(0x2551, Color), nl,
    display_pieces(Color1, Color2, Color), 
    write(' '), put_code_color(0x255A, Color), print_n_code(Size, 0x2550, Color), put_code_color(0x255D, Color), nl, nl.