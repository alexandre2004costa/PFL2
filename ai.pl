count_block([], _, 0).
count_block([Elem | Rest], Block, Count) :-
    count_block(Rest, Block, RestCount),
    (Elem = Block -> Count is RestCount + 1 ; Count is RestCount).

max_count_row([], 9, Block, Count, MaxRow): !.
max_count_row([Row | Rest], NumRow, Block, Count, MaxRow) :-
    count_block(Row, Block, CountRow),
    (CountRow > Count -> Count is CountRow, MaxRow is NumRow)
    max_count_row(Rest, NumRow, Block, Count, MaxRow).

max_count_row([], _, _, CurrentMax, CurrentRow, CurrentMax, CurrentRow). % Base case.
max_count_row([Row | Rest], NumRow, Block, CurrentMax, CurrentRow, Max, MaxRow) :-
    count_block(Row, Block, CountRow),
    (CountRow > CurrentMax ->
        NewMax = CountRow, NewMaxRow = NumRow
    ;
        NewMax = CurrentMax, NewMaxRow = CurrentRow
    ),
    NextRow is NumRow + 1,
    max_count_row(Rest, NextRow, Block, NewMax, NewMaxRow, Max, MaxRow).





value([Player, Board, Levels, OtherPlayer], Player, Value):-
    NumRow is 0, NumCol is 0, 
    max_count_row(Board, NumRow, Block, CountRow, MaxRow),


choose_move([Player, Board, Levels, OtherPlayer], 2, [Piece, Y, X]):-
    valid_moves(Board, Moves),
    choose_best_move([Player, Board, Levels, OtherPlayer], Moves, BestMove),


choose_best_move([Player, Board, Levels, OtherPlayer], [Move|Moves], BestMove):-
    move([Player, Board, Levels, OtherPlayer], Move, NewGameState),
    value(NewGameState, Player, Value),