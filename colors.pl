% display_code(+Char, +Color)
% Displays a character with the specified color and resets the color afterward
display_code('S', Color) :-
    color_code(Color, Code),
    format("~s", [Code]),
    put_code(0x2588), % Full block character.
    reset_color.

display_code('W', Color) :-
    color_code(Color, Code),
    format("~s", [Code]),
    put_code(0x2593), % Medium shade block character.
    reset_color.

display_code('B', Color) :-
    color_code(Color, Code),
    format("~s", [Code]),
    put_code(0x2592), % Dark shade block character.
    reset_color.

% put_code_color(+CharCode, +Color)
% Displays a character specified by its code with the given color and resets the color afterward
put_code_color(C, Color) :-
    color_code(Color, Code),
    format("~s", [Code]),
    put_code(C),
    reset_color.

% write_color(+Text, +Color)
% Writes a string in the specified color and resets the color afterward
write_color(Text, Color) :-
    color_code(Color, Code),
    format("~s~w~s", [Code, Text, "\e[0m"]).

% color_code(+Color, -EscapeCode)
% Maps a color name (Color) to its corresponding escape code for text formatting
color_code(red, "\e[31m").
color_code(green, "\e[32m").
color_code(yellow, "\e[33m").
color_code(blue, "\e[34m").
color_code(magenta, "\e[35m").
color_code(cyan, "\e[36m").
color_code(white, "\e[37m").
color_code(bold_cyan, "\e[1;36m").

% number_to_color(+Number, -Color)
% Maps a number to a predefined color name 
number_to_color(1, red).
number_to_color(2, green).
number_to_color(3, yellow).
number_to_color(4, blue).
number_to_color(5, magenta).
number_to_color(6, cyan).
number_to_color(7, white).
number_to_color(8, bold_cyan).

% reset_color
% Resets the text color to the default terminal color.
reset_color :-
    format("~s", ["\e[0m"]).

% read_input_colors(-Color1, -Color2)
% Reads two color inputs (Color1 and Color2) from the user. Validates the inputs and converts them to color names
read_input_colors(Color1, Color2) :-
    write('Choose color1: '),
    validate_input_colors(Color1),
    write('Choose color2: '),
    validate_input_colors(Color2).

% validate_input_colors(-Color)
% Reads and validates the user input, mapping a valid number to a color name
validate_input_colors(Color) :-
    read(InputN),
    integer(InputN), InputN >= 1, InputN =< 8,
    number_to_color(InputN, Color). % Convert to color

% If the input is invalid, prompts the user again
validate_input_colors(Color) :-
    write('Invalid. Choose a number between 1 and 8. '),
    validate_input_colors(Color).