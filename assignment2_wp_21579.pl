% candidate_number(21579).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

getActor(Actors,ActorIdentity,L):-
  PossibleIdentities = [],
  getActor(Actors,ActorIdentity,PossibleIdentities,L).
getActor(Actors,ActorIdentity):-
  agent_ask_oracle(oscar,o(1),link,L),
  PossibleIdentities = [],
  getActor(Actors,ActorIdentity,PossibleIdentities,L).
getActor(Actors,ActorIdentity,PossibleIdentities,CurrentLink) :-
  Actors = [Head|Tail],
  wp(Head,WT),setof(Link,wt_link(WT,Link),Links),
  ( member(CurrentLink,Links) ->  append(PossibleIdentities,[Head],PossibleIdentities2),
                                    getActor(Tail,ActorIdentity,PossibleIdentities2,CurrentLink)
  ; otherwise                   ->  getActor(Tail,ActorIdentity,PossibleIdentities,CurrentLink)
  ).
getActor([],ActorIdentity,PossibleIdentities,_) :-
  length(PossibleIdentities,Length),
  ( Length =:= 1 -> PossibleIdentities = [Actor],
                    ActorIdentity = Actor
  ; Length  >  1 -> getActor(PossibleIdentities, ActorIdentity)
  ).


find_identity_2(A):-
  setof(X,actor(X),Xs),
  getActor(Xs,ActorIdentity),
  A=ActorIdentity.

find_identity_o(A):-
  my_agent(Agent),
  setof(X,actor(X),Xs),
  find_all_oracles_and_stations(Oracles,Stations),
  query_world( agent_current_position, [Agent,P] ),
  find_distance(Stations,[],FinalNewStations,P),
  writeln("Found Oracles and Charging Stations"),
  find_identity_o(Agent,ActorIdentity,Xs,Oracles,Stations,FinalNewStations),
  A=ActorIdentity.
find_identity_o(Agent,ActorIdentity,Actors,Oracle,Stations,NewStations) :-
  query_world( agent_current_position, [Agent,AgentPosition] ),
  manhattan_oracle(Oracle,[],Final_Manhattan_Oracle,AgentPosition),
  Final_Manhattan_Oracle = [Head|_],
  Head = oracle(_,OraclePosition),
  remove_oracle(Oracle,OraclePosition,[],NewOracleList),
  solve_identity(done(OraclePosition),_,Stations,NewStations,_,FinalNewStations),
  query_world( check_pos, [OraclePosition, X]),
  query_world( agent_ask_oracle, [Agent, X, link, L]),
  getActor2(Actors,ActorIdentity,L,PossibleIdentities),
  length(PossibleIdentities,Length),
  ( Length > 1   -> find_identity_o(Agent,ActorIdentity,PossibleIdentities,NewOracleList,Stations,FinalNewStations)
  ; Length =:= 1 -> true
  ).

remove_oracle(Oracle,OraclePosition,OracleTail,NewOracleList) :-
  Oracle = [Head|Tail],
  ( Head == OraclePosition -> append(Tail,OracleTail,NewOracleList)
  ; otherwise               -> append([Head],OracleTail,NewOracleTail),
                               remove_oracle(Tail,OraclePosition,NewOracleTail,NewOracleList)
  ).
remove_oracle([],_,_,_) :-
  writeln("Could not find identity"),
  fail.

solve_identity(Task,Cost,StationPositions,NewStations,FinalFuelCost,FinalNewStations) :-
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
  FinalFuelCost = FuelCost,
  % check_fake_fuel(StationPositions,FakeBoolean,FuelCost,R,Energy),
  find_distance(StationPositions,[],NewNewStations,P),
  check_fuel(StationPositions,Boolean,FuelCost,R,Energy),
  ( Boolean =:= 1 -> NewNewStations = [Hd|_],
                     Hd = station(_,_,Path),
                     move_agent(Path,Agent),
                     query_world( agent_topup_energy, [Agent, c(_)] ),
                     solve_identity(Task,_,StationPositions,NewStations,_,FinalNewStations)
  ; otherwise     -> move_agent(R,Agent),
                     query_world( agent_current_position, [Agent,NewP] ),
                     find_distance(StationPositions,[],FinalNewStations,NewP)
  ).

check_fuel(Stations,Boolean,FuelCost,R,Energy) :-
  check_fuel(Stations,1,Boolean,FuelCost,R,Energy).
check_fuel(Stations,ChangingBool,Boolean,FuelCost,R,Energy) :-
  Stations = [_|Tail],
  R = [Head2|_],
  length(R,Length),
  ( Length =:= 1 -> MovementAfter = 0
  ; otherwise    -> find_distance(Stations,[],FinalNewStations,Head2),
                    last(FinalNewStations,Last),
                    Last = station(MovementAfter,_,_)
  ),
  ( FuelCost + 10 + MovementAfter > Energy -> NewChangingBool = ChangingBool * 1
  ; otherwise                              -> NewChangingBool = ChangingBool * 0
  ),
  check_fuel(Tail,NewChangingBool,Boolean,FuelCost,R,Energy).
check_fuel([],ChangingBool,Boolean,_,_,_) :-
  Boolean = ChangingBool.

check_fake_fuel(Stations,Boolean,FuelCost,R,Energy) :-
  check_fake_fuel(Stations,1,Boolean,FuelCost,R,Energy).
% check_fake_fuel(Stations,1,Boolean,FuelCost,R,Energy) :-


manhattan_oracle(Oracle,Manhattan_Oracle,Final_Manhattan_Oracle,AgentPosition) :-
  Oracle = [Head|Tail],
  manhattan(Head,AgentPosition,H),
  append([oracle(H,Head)],Manhattan_Oracle,New_Manhattan_Oracle),
  manhattan_oracle(Tail,New_Manhattan_Oracle,Final_Manhattan_Oracle,AgentPosition).
manhattan_oracle([],Manhattan_Oracle,Final_Manhattan_Oracle,_) :-
  sort(Manhattan_Oracle,Final_Manhattan_Oracle).

find_distance(done(X),Cost,FinalFuelCost,Path,P) :-
  check_empty(done(X),Check),
  reachable_pos(Check),
  % query_world( agent_current_position, [Agent,P] ),
  manhattan(P, done(X), H),
  Visited = [],
  D is 0,
  solve_task_bt(done(X),[[c(H,0,P),P]],D,R,Cost,_NewPos,Visited),!,  % prune choice point for efficiency
  Cost = [cost(FuelCost),depth(_)],
  FinalFuelCost = FuelCost,
  Path = R.
find_distance(Stations,NewStations,FinalNewStations,P) :-
  Stations = [Head|Tail],
  find_distance(done(Head),_Cost,FuelCost,Path,P),
  append([station(FuelCost,Head,Path)],NewStations,NewNewStations),
  find_distance(Tail,NewNewStations,FinalNewStations,P).
find_distance([],NewStations,FinalNewStations,_) :-
  sort(NewStations,FinalNewStations).


find_all_oracles_and_stations(Oracles,Stations) :-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  manhattan(P, find(_), H),
  Visited = [],
  D is 0,
  ReturnOracles = [],
  ReturnStations = [],
  Task = find(_),
  find_all_oracles_and_stations(Task,ReturnOracles,ReturnStations,[[c(H,0,P),P]],D,_,_,_NewPos,Visited,FinalOracles,FinalStations),!,
  Oracles = FinalOracles,
  Stations = FinalStations.

find_all_oracles_and_stations(_,Oracles,Stations,[],_,_,_,_,_,FinalOracles,FinalStations) :-
  FinalOracles = Oracles,
  FinalStations = Stations.
find_all_oracles_and_stations(Task,Oracles,Stations,Current,D,RR,Cost,NewPos,Visited,FinalOracles,FinalStations) :-
  Current = [[c(_,G,P)|_]|_],
  P = p(X,Y),
  ( member(visited(X,Y,_),Visited) -> f_is_smaller2(Task,Oracles,Stations,Current,D,RR,Cost,NewPos,Visited,FinalOracles,FinalStations)
  ; otherwise                      -> append([visited(X,Y,G)],Visited,Visited2),
                                      find_all_oracles_and_stations_continued(Task,Oracles,Stations,Current,D,RR,Cost,NewPos,Visited2,FinalOracles,FinalStations)
  ).
find_all_oracles_and_stations_continued(Task,Oracles,Stations,Current,D,R,Cost,NewPos,Visited,FinalOracles,FinalStations) :-
  Current = [[c(_,G,P)|RPath]|Rest],
  findall(R,search(P,_,R,_),L),
  subtract(L,RPath,L2),
  findall(C,map_adjacent(P,C,c(_)),CList),
  findall(O,map_adjacent(P,O,o(_)),OList),
  check_c(CList,Stations,NewStations),
  check_o(OList,Oracles,NewOracles),
  D1 is D+1,
  ( \+nth0(0,L2,_) -> find_all_oracles_and_stations(Task,NewOracles,NewStations,Rest,D,_,Cost,NewPos,Visited,FinalOracles,FinalStations)  % backtrack search
  ; otherwise      -> create_c(L2,L3,Task,G,RPath),
                      append(L3,Rest,L4),
                      sort(L4,NewCurrent),
                      find_all_oracles_and_stations(Task,NewOracles,NewStations,NewCurrent,D1,_,Cost,NewPos,Visited,FinalOracles,FinalStations)  % backtrack search
  ).

check_c(CList,Stations,NewStations) :-
  length(CList,Length),
  ( Length > 0 ->  EmptyList = [],  go_through_c(CList,Stations,NewStations,EmptyList)
  ; otherwise   ->  NewStations = Stations
  ).
go_through_c(CList,Stations,NewStations,EmptyList) :-
  CList = [Head|Tail],
  ( member(Head,Stations) -> go_through_c(Tail,Stations,NewStations,EmptyList)
  ; otherwise             -> append([Head],EmptyList,EmptyList2),
                             go_through_c(Tail,Stations,NewStations,EmptyList2)
  ).
go_through_c([],Stations,NewStations,EmptyList) :-
  append(Stations,EmptyList,NewStations).


check_o(CList,Stations,NewStations) :-
  length(CList,Length),
  ( Length > 0 ->  EmptyList = [],  go_through_o(CList,Stations,NewStations,EmptyList)
  ; otherwise   ->  NewStations = Stations
  ).
go_through_o(CList,Stations,NewStations,EmptyList) :-
  CList = [Head|Tail],
  query_world( check_pos, [Head, O]),
  ( member(Head,Stations) -> go_through_c(Tail,Stations,NewStations,EmptyList)
  ; \+check_oracle_list(O)  -> go_through_c(Tail,Stations,NewStations,EmptyList)
  ; otherwise             -> append([Head],EmptyList,EmptyList2),
                             go_through_c(Tail,Stations,NewStations,EmptyList2)
  ).
go_through_o([],Stations,NewStations,EmptyList) :-
  append(Stations,EmptyList,NewStations).

f_is_smaller2(Task,Oracles,Stations,Current,D,RR,Cost,NewPos,Visited,FinalOracles,FinalStations) :-
  Current = [[c(_,G,P)|_]|Rest],
  P = p(X,Y),
  VisitedRevised = [],
  ( f_is_smaller2(Visited,VisitedRevisedFinal,VisitedRevised,P,G) -> append([visited(X,Y,G)],VisitedRevisedFinal,Visited2),
                                              find_all_oracles_and_stations_continued(Task,Oracles,Stations,Current,D,RR,Cost,NewPos,Visited2,FinalOracles,FinalStations)
  ; otherwise                              -> find_all_oracles_and_stations(Task,Oracles,Stations,Rest,D,RR,Cost,NewPos,Visited,FinalOracles,FinalStations)
  ).
f_is_smaller2(Visited,VisitedRevisedFinal,VisitedRevised,p(X,Y),G) :-
  Visited = [visited(X2,Y2,G2)|Rest],
  ( both_equal(X,X2,Y,Y2)  -> is_G_smaller(G,G2),
                              append(Rest,VisitedRevised,VisitedRevised2),
                              append([visited(X,Y,G)],VisitedRevised2,VisitedRevisedFinal)
  ; otherwise             ->  append(visted(X2,Y2,G2),VisitedRevised,VisitedRevised2),
                              f_is_smaller2(Rest,VisitedRevisedFinal,VisitedRevised2,p(X,Y),G)
  ).
f_is_smaller2([],_,_,_,_) :-
  false.

% is_adjacent(P,P2) :-
%   findall(R,map_adjacent(P,R,_),L),
%   writeln(L),
%   member(P2,L).
  % findall(R,map_adjacent(P,R,empty),L),
  % member(P2,L).
is_adjacent(p(X,Y),p(X2,Y2)) :-
  X3 = (X2 - X) * (X2 - X),
  Y3 = (Y2 - Y) * (Y2 - Y),
  ( X3 =:= 1  -> check_Y(Y3)
  ; Y3 =:= 1  -> check_X(X3)
  ; otherwise -> false
  ).
check_Y(Y) :-
  ( Y =:= 0  -> true
  ; otherwise -> false
  ).
check_X(X) :-
  ( X =:= 0  -> true
  ; otherwise -> false
  ).

getActor2(Actors,ActorIdentity,L,PossibleIdentitiesFinal):-
  PossibleIdentities = [],
  getActor2(Actors,ActorIdentity,PossibleIdentities,L,PossibleIdentitiesFinal).
getActor2(Actors,ActorIdentity,PossibleIdentities,CurrentLink,PossibleIdentitiesFinal) :-
  Actors = [Head|Tail],
  wp(Head,WT),setof(Link,wt_link(WT,Link),Links),
  ( member(CurrentLink,Links) ->  append(PossibleIdentities,[Head],PossibleIdentities2),
                                    getActor2(Tail,ActorIdentity,PossibleIdentities2,CurrentLink,PossibleIdentitiesFinal)
  ; otherwise                   ->  getActor2(Tail,ActorIdentity,PossibleIdentities,CurrentLink,PossibleIdentitiesFinal)
  ).
getActor2([],ActorIdentity,PossibleIdentities,_,PossibleIdentitiesFinal) :-
  length(PossibleIdentities,Length),
  ( Length =:= 1 -> PossibleIdentities = [Actor],
                    PossibleIdentitiesFinal = PossibleIdentities,
                    ActorIdentity = Actor
  ; Length  >  1 -> PossibleIdentitiesFinal = PossibleIdentities
  ; otherwise    -> false
  ).

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
  ( Energy < (FuelCost) -> solve_task2(find(c(_)),_,RefuelPath1,Agent,P),
                         move_agent(RefuelPath1,Agent),
                         query_world( agent_topup_energy, [Agent, c(_)] ),
                         solve_task(go(Head),_)
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
  length(R,Length),
  ( Length =:= 1 -> true
  ; otherwise    -> query_world( agent_do_moves, [Agent,Path] )
  ).



%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos,_) :-
  % writeln(Task),
  % writeln("this first"),
  achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(_,[],_,_,_,_,_) :-
  fail.
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

achieved(done(Exit),Current,RPath,Cost,NewPos) :-
  Current = [[c(_,Cost,NewPos)|RPath]|_],
  RPath = [Last|_],
  ( Exit=none -> true
  ; otherwise              -> is_adjacent(Exit,Last)
  ).
achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [[c(_,Cost,NewPos)|RPath]|_],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(o(X)),Current,RPath,Cost,NewPos) :-
  Current = [[c(_,Cost,NewPos)|RPath]|_],
  O = o(X),
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O),check_oracle_list(O),map_adjacent(Last,_,O)
  ).
achieved(find(c(X)),Current,RPath,Cost,NewPos) :-
  Current = [[c(_,Cost,NewPos)|RPath]|_],
  O = c(X),
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

check_oracle_list(o(X)) :-
  my_agent(Agent),
  ( query_world( agent_check_oracle, [Agent, o(X)]) -> false
  ; otherwise                    -> true
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).

manhattan(p(X,Y), p(X1,Y1), H) :-
  H is (abs(X-X1) + abs(Y-Y1)).
manhattan(p(X,Y), go(p(X1,Y1)), H) :-
  H is (abs(X-X1) + abs(Y-Y1)).
manhattan(p(X,Y), done(p(X1,Y1)), H) :-
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
check_empty(done(_),_) :-
  true.

reachable_pos(t(_)) :-
  false.
reachable_pos(empty) :-
  true.
reachable_pos(o(_)) :-
  true.
reachable_pos(c(_)) :-
  true.
