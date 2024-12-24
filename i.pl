at([Value|_], 0, Value).
at([_|Rest], X, Value):- 
    X > 0,
    X1 is X - 1,
    at(Rest, X1, Value).

get_value([Row|_], 0, X, Value):- 
    at(Row, X, Value).

get_value([_|Rest], Y, X, Value):- 
    Y > 0,
    Y1 is Y - 1,
    get_value(Rest, Y1, X, Value).

find_positions(Value, List, Positions) :-
    findall(Index, nth0(Index, List, Value), Positions).