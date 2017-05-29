goal(state(_,_,_,has)).

init(state(door,floor,window,not)).

move(state(middle,box,middle,not),
     grasp,
     state(middle,box,middle,has)).
move(state(Pos,floor,Pos,Has),
     climb,
     state(Pos,box,Pos,Has)).
move(state(L1,floor,L1,Has),
     push(L1,L2),
     state(L2,floor,L2,Has)).
move(state(L1,floor,Pos,Has),
     walk(L1,L2),
     state(L2,floor,Pos,Has)).

solve(State) :- goal(State).
solve(State1) :- move(State1, Move, State2), solve(State2).

solver(L) :- init(S), solve(S,L).

solve(State,[])  :- goal(State).
solve(State1,[Move|L]) :- move(State1, Move, State2), print(State1), nl(), solve(State2,L).