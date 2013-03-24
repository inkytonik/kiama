male(albert).
male(edward).
male(bob).
female(alice).
female(victoria).

parent(edward,victoria).
parent(edward,albert).
parent(alice,albert).
parent(alice,victoria).
parent(alice,bob).

father(F,C) :- male(F), parent(F,C).
mother(M,C) :- female(M), parent(M,C).

son(S,P) :- male(S), parent(P,S).
daughter(D,P) :- female(D), parent(P,D).
