#|

Exercise 3.49: Give a scenario where the deadlock-avoid-ance
mechanism described above does not work. (Hint: In the
exchange problem, each process knows in advance which
accounts it will need to get access to. Consider a situation
where a process must get access to some shared resources
before it can know which additional shared resources it will
require.)

|#

#| Answer 

An operation may not know every resource it needs ahead of time. This is not
necessarily a problem. A resource could take ids (4 5) and later determine that
it needs other ids (1 2). Everything is fine only if it releases all resources
before requesting additional resources (in id order) for the next stage of the
operation. It can even re-take (4 5) if needed.

There is an issue if the operation can't release those resources before moving
on to the next stage. For example, those resources may be left in some invalid
state which can't be resolved without checking out additional resources.

In many cases this can be coded around.

|#
