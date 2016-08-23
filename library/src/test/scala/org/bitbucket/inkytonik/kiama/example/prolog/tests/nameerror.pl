female(mary).        
likes(john,X) :- likes(X,wine), likes(X,food,beer).
likes(john) :- female(X,likes), doesntlike(X,wine).
beer(john).
