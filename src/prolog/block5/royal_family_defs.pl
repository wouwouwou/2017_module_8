father(X,Y) :- husband(X,Z), mother(Z,Y).

parent(P,C) :- father(P,C); mother(P,C).
child(C,P) :- parent(P,C).

grandparent(GP,C) :- parent(GP,P), parent(P,C).
sibling(S,C) :- parent(P,S), not(S=C), parent(P,C).

brother(B,C) :- sibling(B,C), male(B).
sister(S,C) :- sibling(S,C), female(S).

aunt(A,C) :- parent(P,C), sister(A,P).
uncle(U,C) :- parent(P,C), brother(U,P).

cousin(Co,C) :- parent(P,C), sibling(T,P), child(Co,T), male(Co).
nephew(N,C) :- sibling(P,C), child(N,P).

ancestor(A,C) :- parent(A,C).
ancestor(A,C) :- parent(P,C), ancestor(A,P).