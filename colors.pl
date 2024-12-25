display_code('S', Color) :-
    color_code(Color, Code),
    format("~s", [Code]),
    put_code(0x2588),
    reset_color.

display_code('W', Color) :-
    color_code(Color, Code),
    format("~s", [Code]),
    put_code(0x2593),
    reset_color.

display_code('B', Color) :-
    color_code(Color, Code),
    format("~s", [Code]),
    put_code(0x2592),
    reset_color.

write_color(Text, Color) :-
    color_code(Color, Code),
    format("~s~w~s", [Code, Text, "\e[0m"]).

% basic color
color_code(red, "\e[31m").
color_code(green, "\e[32m").
color_code(yellow, "\e[33m").
color_code(blue, "\e[34m").
color_code(magenta, "\e[35m").
color_code(cyan, "\e[36m").
color_code(white, "\e[37m").
color_code(bold_cyan, "\e[1;36m").

% background color
color_code(bg_black, "\e[40m").
color_code(bg_red, "\e[41m").
color_code(bg_green, "\e[42m").
color_code(bg_yellow, "\e[43m").
color_code(bg_blue, "\e[44m").
color_code(bg_magenta, "\e[45m").
color_code(bg_cyan, "\e[46m").
color_code(bg_white, "\e[47m").


reset_color :-
    format("~s", ["\e[0m"]).


read_input_colors(Color1, Color2) :-
    write('Choose color1: '),
    validate_input_colors(Color1),
    validate_input_colors(Color2).    
    
validate_input_colors(N):-
    read(InputN),
    integer(InputN), InputN >= 1, InputN =< 9, N = InputN, !.
validate_input_colors(N) :-
    write('Invalid. Choose a number between 1 and 9. '),
    validate_input_colors(N).