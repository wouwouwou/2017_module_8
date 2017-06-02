a0.
a1.
a2.
a2 :- d.
d :- false.

b0 :- a0, a1.
b1 :- a1, a2.
b2 :- a1, a2, d.

c0 :- b0, b1.
c1 :- b0, b1, b2.