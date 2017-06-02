/*
                             Logic Problems
                         ----------------------
                            Ice Cream Stands
                            by Shelly Hazard

1a Sally’s Ice Cream wasn’t in Rockland.
1b Sherry didn’t get peppermint stick ice cream on Thursday night.

2  Sherry stopped at the ice cream stand in Granite on the day
   before she got the chocolate chip ice cream and the day after she
   stopped at Gary’s Ice Cream.

3  At Tom’s Ice Cream she got peanut butter ice cream but not on Tuesday.

4  She got coffee bean ice cream on Wednesday, but not at Alice’s Ice Cream.

5  She stopped at the stand in Marsh the day before she stopped at Sally’s Ice Cream.

6  The ice Sherry got last night in Boulder was gorgeous as well.

*/

% Helper predicate to check if all elements of [H|T] are in L2
all([H],L2) :- member(H,L2).
all([H|T],L2) :- member(H,L2), all(T, L2).

% Helper predicate to check if X and Y are ordered next to each other.
right(X,Y,L) :- append(_,[X,Y|_],L).

solve(Sol) :-
  Sol=[[A, A_loc, tuesday, A_flv],
       [B, B_loc, wednesday, B_flv],
       [C, C_loc, thursday, C_flv],
       [D, D_loc, friday, D_flv]],

  % Make sure that all stands, locations and flavours are present.
  all([alice, gray, tom, sally], [A, B, C, D]),
  all([boulder, granite, marsh, rockland], [A_loc, B_loc, C_loc, D_loc]),
  all([chocolate, coffee, peanut, peppermint], [A_flv, B_flv, C_flv, D_flv]),

  % 1a
  member([sally, C1_loc, _, _], Sol),
  % 1b
  member([_, _, thursday, C1_flav], Sol),

  % 2
  right([_, granite, _, _], [_, _, _, chocolate], Sol),
  right([gray, _, _, _], [_, granite, _, _], Sol),

  % 3
  member([tom, _, C3_day, peanut], Sol),

  % 4
  member([C4_stand, _, wednesday, coffee], Sol),

  % 5
  right([_, marsh, _, _],[sally, _, _, _], Sol),

  % 6
  % member([alice, boulder, C6_day, _], Sol),

  not(C1_loc = rockland),     % 1a
  not(C1_flav = peppermint),  % 1b
  not(C3_day = tuesday),      % 3
  % not(C6_day = friday),       % 6
  not(C4_stand = alice).      % 4

go(Sol) :- solve(Sol).
