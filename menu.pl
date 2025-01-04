% Menu ---------------------------------------------------------------------------------------------------------------------------
 
% state(+CurrentState, +Color1, +Color2)
% Handles the current state of the game and transitions to the next state based on user input or game logic

% Handles the initial state, allowing the user to choose to play, exit or change configurations
state(initial, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_menu(Color1, Color2), 
    read_option(3, Option),
    transition(initial, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle). 

% Handles the mode state, allowing the user to choose a game mode
state(mode, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_play,  
    read_option(4, Option),
    transition(mode, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle). 

% Handles the config state, allowing the user to change some configurations
state(config, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_config,  
    read_option(4, Option),
    transition(config, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle). 

% Handles the color state, allowing the user to choose custom colors for the game
state(colors, _, _, _, _):-
    print_banner_colors(1),
    print_banner_colors(2), nl,
    read_input_colors(Color11, Color22),
    print_banner_display_colors(Color11, Color22), nl,
    state(initial, Color11, Color22, _, _).

% Handles the board_size state, allowing the user to change the board size
state(board_size, Color1, Color2, _, BoardStyle):-
    print_banner_board_size,
    read_option(2, Option),
    state(initial, Color1, Color2, Option, BoardStyle). 

% Handles the board_style state, allowing the user to change the board style
state(board_style, Color1, Color2, BoardSize, _):-
    print_banner_board_style,
    read_option(3, Option),
    state(initial, Color1, Color2, BoardSize, Option). 


% Handles the state for a player vs computer game mode
state(play_uc, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_level,
    read_option(2, Option),
    transition(play_uc, Option, NextState), 
    state(NextState, Option, Color1, Color2, BoardSize, BoardStyle).

% Handles the state for a computer vs computer game mode
state(play_cc, Color1, Color2, BoardSize, BoardStyle) :-
    print_banner_pc,
    read_option(5, Option),
    transition(play_cc, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle).

% Handles the state when a winner is declared
state(winner, Color1, Color2, BoardSize, BoardStyle) :-
    read_option(2, Option),
    transition(winner, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle).

% Handles the exit state, where the game ends.
state(exit, _, _, _, _) :-
    write('Exiting...'), nl, nl. 

% state(+CurrentState, +LastOption, +Color1, +Color2)
% Handles the state where the player chooses whether they or the computer starts
state(play_uc_choose_start, LastOption, Color1, Color2, BoardSize, BoardStyle):-
    print_banner_starter,
    read_option(2, Option),
    transition(play_uc_choose_start, LastOption, Option, NextState), 
    state(NextState, Color1, Color2, BoardSize, BoardStyle).

% State handlers for different levels of the game
% Each level represents a specific game mode configuration
state(play_uu, Color1, Color2, BoardSize, BoardStyle) :- play_game('PlayerVsPlayer', Color1, Color2, BoardSize, BoardStyle).
state(levelUC11, Color1, Color2, BoardSize, BoardStyle) :- play_game('PlayerVsPc_1', Color1, Color2, BoardSize, BoardStyle).
state(levelUC12, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_1VsPlayer', Color1, Color2, BoardSize, BoardStyle).
state(levelUC21, Color1, Color2, BoardSize, BoardStyle) :- play_game('PlayerVsPc_2', Color1, Color2, BoardSize, BoardStyle).
state(levelUC22, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_2VsPlayer', Color1, Color2, BoardSize, BoardStyle).
state(levelCC11, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_1VsPc_1', Color1, Color2, BoardSize, BoardStyle).
state(levelCC12, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_1VsPc_2', Color1, Color2, BoardSize, BoardStyle).
state(levelCC21, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_2VsPc_1', Color1, Color2, BoardSize, BoardStyle).
state(levelCC22, Color1, Color2, BoardSize, BoardStyle) :- play_game('Pc_2VsPc_2', Color1, Color2, BoardSize, BoardStyle).

% transition(+CurrentState, +Option, -NextState)
% Defines state transitions based on the current state and user input
transition(initial, 1, mode).  
transition(initial, 2, config).  
transition(initial, 3, exit).  
transition(mode, 1, play_uu).  
transition(mode, 2, play_uc).  
transition(mode, 3, play_cc).
transition(mode, 4, initial).
transition(config, 1, colors).
transition(config, 2, board_size).
transition(config, 3, board_style).
transition(config, 4, initial).
transition(play_uc, _, play_uc_choose_start).
transition(play_uc_choose_start, 1, 1, levelUC11).
transition(play_uc_choose_start, 1, 2, levelUC12).
transition(play_uc_choose_start, 2, 1, levelUC21).
transition(play_uc_choose_start, 2, 2, levelUC22).
transition(play_cc, 1, levelCC11).
transition(play_cc, 2, levelCC12).
transition(play_cc, 3, levelCC21).
transition(play_cc, 4, levelCC22).
transition(play_cc, 5, mode).
transition(winner, 1, initial).
transition(winner, 2, exit).
transition(_, _, initial).
