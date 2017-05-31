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

% Question 1: alphabet/2

%alphabet(nfa(_, T, _), L) :- L = Z, labels(T, Z).

%labels([],_).
%labels([T|Ts], Z) := labels(Ts, [label(T)|Z]).

% Try this: example(n3, N), isnfa(N), init(N, I),
%           transitions(N, Ts), accepting(N, A).

init(nfa(Init, _, _), I) :- I = Init.
accepting(nfa(_, _, Accepting), A) :- A = Accepting.
transitions(nfa(_, Transitions, _), Ts) :- Ts = Transitions.

isnfa(nil).
isnfa(nfa(Init, Transitions, Accepting)) :-
  isState(Init), areTransitions(Transitions), areStates(Accepting).

isState(nil) :- fail.
isState(S) :- integer(S).

areTransitions([]).
areTransitions([T|Ts]) :- isTransistion(T), areTransitions(Ts).

isTransistion(nil) :- fail.
isTransistion(trans(S1, L, S2)) :- isState(S1), is_alpha(L), isState(S2).

areStates([]).
areStates([S|States]) :- isState(S), areStates(States).


% Question 2: testNFA/2

% ...

% Operators for regular expressions

:- op(650,xfy,+).
:- op(640,xfy,^).
:- op(630,xf,*).

exampleRE(e1,(a+a^a)^(a^a^a)*).
exampleRE(e2,(a^a+b)*).

% Question 3: count/2

% ...

% Question 4: testRE/2

% ...
