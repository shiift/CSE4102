/* collapses sequences of consecutive identical ground terms  */
compress([], []).
compress([X|[X|Zs]], Y) :- !, compress([X|Zs], Y).
compress([X|Xs], [X|Ys]) :- compress(Xs, Ys).

/* collapses lists into a single */
my_flatten([], []) :- !.
my_flatten([X|Xs], Y) :- !, my_flatten(X, T1), my_flatten(Xs, T2), append(T1, T2, Y).
my_flatten(X, [X]).

/* packs the list based on repeating values */
pack([X], [[X]]) :- !.
pack([X|[X|Xs]], [[X|Zs]|Y]) :- !, pack([X|Xs], [Zs|Y]).
pack([X|[Z|Zs]], [[X]|Ys]) :- pack([Z|Zs], Ys).

/* count the packed terms */
rlencode(X,Y) :- pack(X,T), rlencode2(T,Y).
rlencode2([], []).
rlencode2([[X|Xs]|Ys], [[X, Z]|Zs]) :- length([X|Xs], Z), rlencode2(Ys, Zs).

/* create terms in given range and return list */
range(X, X, [X]) :- !.
range(X, Y, [X|Ls]) :- !, Z is X + 1, range(Z, Y, Ls).
