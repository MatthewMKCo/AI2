# Grid World assignment #
This repository contains *Grid World* assignment for University of Bristol [COMS30106](https://COMS30106.github.io/) course.

The coursework description is available in wiki format [here](https://github.com/COMS30106/assignment/wiki).

length(ActorWithLinks,Length),
( Length =:= 1 -> ActorWithLinks = [actorandlinks(Actor,ActorLinks)],
                                   ActorIdentity = Actor
; otherwise    -> ActorWithLinks = [actorandlinks(Actor,ActorLinks)|Rest],
                                   ActorIdentity = Actor
).

(20,20)(1,1)(20,20)(20,1)(1,20)(20,1)

(8,1)
topup(c(_))
(20,20)
(20,1)
(1,20)
(20,1)
