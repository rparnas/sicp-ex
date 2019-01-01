#|

Exercise 4.76: Our implementation of "and" as a series
combination of queries (Figure 4.5) is elegant, but it is
inefficient because in processing the second query of the
"and" we must scan the data base for each frame produced by
the first query. If the data base has n elements, and a
typical query produces a number of output frames
proportional to n (say n / k), then scanning the data base
for each frame produced by the first query will require n^2
/ k calls to the pattern matcher. Another approach would be
to process the two clauses of the "and" separately, then
look for all pairs of output frames that are compatible. If
each query produces n / k output frames, then this means
that we must perform n^2 / k^2 compatibility checks---a
factor of k fewer than the number of matches required in our
current method.

Devise an implementation of "and" that uses this strategy.
You must implement a procedure that takes two frames as
inputs, checks whether the bindings in the frames are
compatible, and, if so, produces a frame that merges the two
sets of bindings. This operation is similar to unification.

|#

