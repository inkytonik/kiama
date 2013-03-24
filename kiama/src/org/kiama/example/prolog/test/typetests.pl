pa(1).
pa(X) :- pa(X).

pb(1).
pb([1]) :- pb(a).

pc(a).
pc(X) :- pc(X).

pd(a).
pd(1) :- pd([1]).

pe([1]).
pe(X) :- pe(X).

pf([1]).
pf(a) :- pf(1).

pg(1,[1]).
pg(X,Y).
pg(Y,X).

ph (1).
ph (X) :- pi (X).
pi (Y) :- pi (a).

pj (1).
pk (1,[1]) :- pj (X), pk (X, X).
