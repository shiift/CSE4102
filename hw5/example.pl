squint([], []).
squint([X|Xs], [Y|Ys]) :- integer(X), !, Y is X * X, squint(Xs, Ys).
squint([X|Xs], [Y|Ys]) :- X = Y, squint(Xs, Ys).

