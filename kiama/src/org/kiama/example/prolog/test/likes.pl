likes(mary,food).
likes(mary,wine).
likes(john,wine).
likes(john,mary).

likes(john,X) :- likes(X,wine), likes(X,food).
likes(john,X) :- female(X), likes(X,wine).
