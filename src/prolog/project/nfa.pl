% An NFA is a triple of the form:
% nfa(Init, Transitions, Accepting), where:
% - Init is a state,
% - Transitions a list of triples of the form trans(State,Label,State)
% - Accepting a list of accepting states.


% example n1 recognizes the language a+ . b+
%
example(n1, nfa(0,[trans(0,a,0), trans(0,a,1),trans(1,b,1),trans(1,b,2)], [2]) ).

% example n2 recognizes words that contain aaa or bbb
%
example(n2, nfa(0,
 [trans(0,a,0),trans(0,a,1),trans(1,a,2),trans(2,a,5),trans(5,a,5),
  trans(0,b,0),trans(0,b,3),trans(3,b,4),trans(4,b,5),trans(5,b,5)],
 [5])).

% example n3 equals n1, but also accepts state 1.
%
example(n3, nfa(0,[trans(0,a,0), trans(0,a,1),trans(1,b,1),trans(1,b,2)], [1, 2]) ).

% example n4 equals n3, but is not a valid nfa.
%
example(n4, nfa(b,[trans(0,a,0), trans(0,a,1),trans(1,b,1),trans(1,b,2)], [1, 2]) ).

% Helper functions. We like them and they are nice to use.
% Try this: example(n3, N), isnfa(N), init(N, I),
%           transitions(N, Ts), accepting(N, A).

init(nfa(Init, _, _), I) :- I = Init.
accepting(nfa(_, _, Accepting), A) :- A = Accepting.
transitions(nfa(_, Transitions, _), Ts) :- Ts = Transitions.

labels([], []) :- !.
labels([trans(_, Label, _)|Ts], Ls) :- labels(Ts, T), list_to_set([Label|T], Ls).

from(trans(From, _, _), F) :- F = From.
label(trans(_, Label, _), L) :- L = Label.
to(trans(_, _, To), T) :- T = To.

isnfa(nil).
isnfa(nfa(Init, Transitions, Accepting)) :-
  isState(Init), areTransitions(Transitions), areStates(Accepting).

isState(nil) :- fail.
isState(S) :- integer(S).

areTransitions([]).
areTransitions([T|Ts]) :- isTransistion(T), areTransitions(Ts).

isTransition(nil) :- fail.
isTransition(trans(S1, L, S2)) :- isState(S1), is_alpha(L), isState(S2).

areStates([]).
areStates([S|States]) :- isState(S), areStates(States).

% Question 1: alphabet/2

alphabet(N, L) :- transitions(N, Ts), labels(Ts, L).


% Question 2: testNFA/2

testNFA([], nfa(Init, _, Accepting)) :- !,  member(Init, Accepting).
testNFA([S|Ss], nfa(Init, Transitions, Accepting)) :- findTrans(Init, S, Transitions, To),
  testNFA(Ss, nfa(To, Transitions, Accepting)),!.

findTrans(Init, S, [trans(Init, S, To)|_], To).
findTrans(Init, S, [_|Ts], To) :- findTrans(Init, S, Ts, To).


% Operators for regular expressions

:- op(650,xfy,+).
:- op(640,xfy,^).
:- op(630,xf,*).

exampleRE(e1,(a+a^a)^(a^a^a)*).
exampleRE(e2,(a^a+b)*).

% Question 3: count/2

count(C, 0) :- atom(C).
count(C+D, O) :- count(C, N), count(D, M), O is M+N+1.
count(C^D, O) :- count(C, N), count(D, M), O is M+N+1.
count(C*, O) :- count(C, N), O is N+1.

% Question 4: testRE/2

testRE(S, E) :- makeNFA(E, N), testNFA(S, N).

makeNFA(C, nfa(0, [trans(0, C, 1)], [1])) :- atom(C).
makeNFA(C+D, N) :- makeNFA(C, CN), makeNFA(D, DN), orNFA(CN, DN, N).
makeNFA(C^D, N) :- makeNFA(C, CN), makeNFA(D, DN), concNFA(CN, DN, N).
makeNFA(C*, N) :- makeNFA(C, CN), kleeneNFA(CN, N).

orNFA(CN, DN, N) :- !.

concNFA(CN, DN, N) :- !.

kleeneNFA(CN, N) :- beginState(CN, Begin), endState(CN, End), New is End + 1,
 reform(End, New, CN, CN2), reform(Begin, New, CN2, N).

reform(OldS, NewS, nfa(InitOld, TransitionsOld, AcceptingOld),
 nfa(InitNew, TransitionsNew, AcceptingNew)) :- !.

beginState(nfa(_, Transitions, _), S) :- my_min(Transitions, S))
endState(nfa(_, Transitions, _), S) :- my_max(Transitions, S).

my_max([trans(F, _, T)|Ss], M) :- my_max(Ss, N), M is max(N,max(F, T)).
my_min([trans(F, _, T)|Ss], M) :- my_min(Ss, N), M is min(N,min(F, T)))
