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

% Base Case with one character
makeNFA(C, nfa(0, [trans(0, C, 1)], [1])) :- atom(C).

% Or operator (+)
makeNFA(C+D, N)
    :-  makeNFA(C, nfa(C_Init, C_Edges, [C_Final|_])),
        makeNFA(D, D_N),
        nfa_max(C_Edges, Offset),
        translate_states(Offset + 1, D_N, nfa(D_Init, D_Edges, [D_Final|_])),
        append(C_Edges, D_Edges, F_Edges),
        reform(D_Init, C_Init, nfa(C_Init, F_Edges, [C_Final]), Z),
        reform(D_Final, C_Final, Z, N).

% String Concatenation
makeNFA(C^D, N)
    :-  makeNFA(C, nfa(C_Init, C_Edges, [C_Final|_])),
        makeNFA(D, D_N),
        nfa_max(C_Edges, Offset),
        translate_states(Offset + 1, D_N, nfa(D_Init, D_Edges, [D_Final|_])),
        append(C_Edges, D_Edges, F_Edges),
        reform(D_Init, C_Final, nfa(C_Init, F_Edges, [D_Final]), N).

% Kleene Star
makeNFA(C*, N)
    :-  makeNFA(C, nfa(C_Init, C_Edges, [C_Final|_])),
        reform(C_Init, C_Final, nfa(C_Init, C_Edges, [C_Final]), N).

% Base case reform function.
reform(_, _,
    nfa(InitOld, [], AcceptingOld),
    nfa(InitOld, [], AcceptingOld))
        :- !.

% Transform initial and accepting states. The first function transforms the
% initial state to the new state. The second function transforms the
% old accepting states to the new state number.
reform(OldS, NewS,
    nfa(OldS, TransitionsOld, AcceptingOld),
    nfa(NewS, TransitionsNew, AcceptingNew))
        :-  reform(OldS, NewS,
                nfa(NewS, TransitionsOld, AcceptingOld),
                nfa(NewS, TransitionsNew, AcceptingNew)), !.

reform(OldS, NewS,
    nfa(InitOld, TransitionsOld, AcceptingOld),
    nfa(InitNew, TransitionsNew, AcceptingSel))
        :-  member(OldS, AcceptingOld),
            select(OldS, AcceptingOld, NewS, AcceptingSel),
            reform(OldS, NewS,
                nfa(InitOld, TransitionsOld, AcceptingSel),
                nfa(InitNew, TransitionsNew, AcceptingSel)), !.

% Transforms the transitions, such that all present old states will be
% replaced by the new state.
reform(OldS, NewS,
    nfa(InitOld, [trans(From,Char,To)|Transitions], AcceptingOld),
    nfa(InitNew, [trans(FromNew, Char, ToNew)|TransitionsNew], AcceptingNew))
        :-  ((OldS =:= From, FromNew is NewS); (FromNew is From)),
            ((OldS =:= To, ToNew is NewS); (ToNew is To)),
            reform(OldS, NewS,
                nfa(InitOld, Transitions, AcceptingOld),
                nfa(InitNew, TransitionsNew, AcceptingNew)), !.


% Adds an offset to all states.
translate_states(_, nfa(InitOld, [], AcceptingOld), nfa(InitOld, [], AcceptingOld))
        :-  !.

translate_states(Offset,
    nfa(InitOld, [trans(From,Char,To)|Transitions], [AcceptingOld|_]),
    nfa(InitNew, [trans(FromNew,Char,ToNew)|TransitionsNew], [AcceptingNew]))
        :-  InitNew is InitOld + Offset,
            AcceptingNew is AcceptingOld + Offset,
            FromNew is From + Offset,
            ToNew is To + Offset,
            translate_states(Offset, nfa(InitOld, Transitions, [AcceptingOld]), nfa(_, TransitionsNew, _)).

% Helper functions for finding minima and maxima.
minState(nfa(_, Transitions, _), S) :- nfa_min(Transitions, S), !.
maxState(nfa(_, Transitions, _), S) :- nfa_max(Transitions, S), !.

nfa_max([trans(F, _, T)], M) :- M is max(F, T), !.
nfa_max([trans(F, _, T)|Ss], M) :- nfa_max(Ss, N), M is max(N,max(F, T)).

nfa_min([trans(F, _, T)], M) :- M is min(F, T), !.
nfa_min([trans(F, _, T)|Ss], M) :- nfa_min(Ss, N), M is min(N,min(F, T)).
