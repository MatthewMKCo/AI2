candidate_number(21579).

solve_task(Task,Cost):-
  check_empty(Task,Check),
  reachable_pos(Check),
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  query_world( agent_current_energy, [Agent,Energy]),
  manhattan(P, Task, H),
  Visited = [],
  D is 0,
  solve_task_bt(Task,[[c(H,0,P),P]],D,R,Cost,_NewPos,Visited),!,  % prune choice point for efficiency
  Cost = [cost(FuelCost),depth(_)],
  R = [Head|_],
  % solve_task2(),
  ( Energy < FuelCost -> solve_task2(find(c(_)),_,RefuelPath1,Agent,P),
                         move_agent(RefuelPath1,Agent),
                         query_world( agent_topup_energy, [Agent, c(_)] ),
                         solve_task(Task,_)
  ; otherwise         -> solve_task2(find(c(_)),Cost2,_,Agent,Head),
                         Cost2 = [cost(FuelCost2),depth(_)],
                         move_agent2(Task,Energy,FuelCost,FuelCost2,Agent,P,R)
  ).

move_agent2(Task,Energy,Cost,Cost2,Agent,P,R) :-
  ( Energy < (Cost + Cost2) -> solve_task2(find(c(_)),_,RefuelPath,Agent,P),
                               move_agent(RefuelPath,Agent),
                               query_world( agent_topup_energy, [Agent, c(_)] ),
                               solve_task(Task,_)
  ; otherwise               -> move_agent(R,Agent)
  ).

solve_task2(Task,Cost,R,Agent,P2):-
  check_empty(Task,Check),
  reachable_pos(Check),
  query_world( agent_current_position, [Agent,P] ),
  manhattan(P, Task, H),
  Visited = [],
  D is 0,
  solve_task_bt(Task,[[c(H,0,P2),P2]],D,R,Cost,_NewPos,Visited),!.  % prune choice point for efficiency

move_agent(R,Agent) :-
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).



%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos,_) :-
  achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos,Visited) :-
  Current = [[c(_,G,P)|_]|_],
  P = p(X,Y),
  ( member(visited(X,Y,_),Visited) -> f_is_smaller(Task,Current,D,RR,Cost,NewPos,Visited)
  ; otherwise                      -> append([visited(X,Y,G)],Visited,Visited2),
                                      solve_task_bt_continued(Task,Current,D,RR,Cost,NewPos,Visited2)
  ).
solve_task_bt_continued(Task,Current,D,RR,Cost,NewPos,Visited) :-
  Current = [[c(_,G,P)|RPath]|Rest],
  findall(R,search(P,_,R,_),L),
  subtract(L,RPath,L2),
  D1 is D+1,
  ( \+nth0(0,L2,_) -> solve_task_bt(Task,Rest,D,RR,Cost,NewPos,Visited)  % backtrack search
  ; otherwise      -> create_c(L2,L3,Task,G,RPath),
                      append(L3,Rest,L4),
                      sort(L4,NewCurrent),
                      solve_task_bt(Task,NewCurrent,D1,RR,Cost,NewPos,Visited)  % backtrack search
  ).

f_is_smaller(Task,Current,D,RR,Cost,NewPos,Visited) :-
  Current = [[c(_,G,P)|_]|Rest],
  P = p(X,Y),
  VisitedRevised = [],
  ( f_is_smaller(Visited,VisitedRevisedFinal,VisitedRevised,P,G) -> append([visited(X,Y,G)],VisitedRevisedFinal,Visited2),
                                              solve_task_bt_continued(Task,Current,D,RR,Cost,NewPos,Visited2)
  ; otherwise                              -> solve_task_bt(Task,Rest,D,RR,Cost,NewPos,Visited)
  ).
f_is_smaller(Visited,VisitedRevisedFinal,VisitedRevised,p(X,Y),G) :-
  Visited = [visited(X2,Y2,G2)|Rest],
  ( both_equal(X,X2,Y,Y2)  -> is_G_smaller(G,G2),
                              append(Rest,VisitedRevised,VisitedRevised2),
                              append([visited(X,Y,G)],VisitedRevised2,VisitedRevisedFinal)
  ; otherwise             ->  append(visted(X2,Y2,G2),VisitedRevised,VisitedRevised2),
                              f_is_smaller(Rest,VisitedRevisedFinal,VisitedRevised2,p(X,Y),G)
  ).
f_is_smaller([],_) :-
  false.

both_equal(X,X2,Y,Y2) :-
  X =:= X2,
  Y =:= Y2.
is_G_smaller(G,G2) :-
  G < G2.

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [[c(_,Cost,NewPos)|RPath]|_],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(o(X)),Current,RPath,Cost,NewPos) :-
  Current = [[c(_,Cost,NewPos)|RPath]|_],
  O = o(X),
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).
achieved(find(c(X)),Current,RPath,Cost,NewPos) :-
  Current = [[c(_,Cost,NewPos)|RPath]|_],
  O = c(X),
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).

manhattan(p(X,Y), go(p(X1,Y1)), H) :-
  H is (abs(X-X1) + abs(Y-Y1)).
manhattan(p(_,_), find(_), H) :-
  H is 0.

create_c(List1,FinalList,Task,G,RPath) :-
  List1 = [Pos|Rest],
  manhattan(Pos,Task,H),
  G1 is G+1,
  F is G1+H,
  NewList1 = Rest,
  create_c(NewList1,[[c(F,G1,Pos),Pos|RPath]],FinalList,Task,G,RPath).
create_c(List1,List2,FinalList,Task,G,RPath) :-
  List1 = [Pos|Rest],
  manhattan(Pos,Task,H),
  G1 is G+1,
  F is G1+H,
  NewList1 = Rest,
  append(List2,[[c(F,G1,Pos),Pos|RPath]],NewList2),
  create_c(NewList1,NewList2,FinalList,Task,G,RPath).
create_c([],List2,FinalList,_,_,_) :-
  sort(List2,FinalList).

check_empty(go(p(X,Y)),Check) :-
  query_world( check_pos, [p(X,Y), Check]).
check_empty(find(_),_) :-
  true.

reachable_pos(t(_)) :-
  false.
reachable_pos(empty) :-
  true.
reachable_pos(o(_)) :-
  true.
reachable_pos(c(_)) :-
  true.
