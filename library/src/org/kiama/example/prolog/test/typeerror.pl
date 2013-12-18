next(0,1).
less(0,1).
less(X,Y) :- next(X,tony).
less([1,2],Y) :- next(X,W), less(W,Y).

length([],0).
length(L,N) :- split(L,H,T), length(T,M), next(M,N).
