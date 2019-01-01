#|

Exercise 3.70: It would be nice to be able to generate
streams in which the pairs appear in some useful order,
rather than in the order that results from an ad hoc
interleaving process. We can use a technique similar to the
"merge" procedure of Exercise 3.56, if we define a way to
say that one pair of integers is "less than" another. One
way to do this is to define a "weighting function" W(i, j)
and stipulate that (i_1, j_1) is less than (i_2, j_2) if
W(i_1, j_1) < W(i_2, j_2). Write a procedure
"merge-weighted" that is like "merge", except that
"merge-weighted" takes an additional argument "weight",
which is a procedure that computes the weight of a pair, and
is used to determine the order in which elements should
appear in the resulting merged stream. Using this,
generalize "pairs" to a procedure "weighted-pairs" that
takes two streams, together with a procedure that computes a
weighting function, and generates the stream of pairs,
ordered according to weight. Use your procedure to generate

a. the stream of all pairs of positive integers (i, j) with
i <= j ordered according to the sum i + j,

b. the stream of all pairs of positive integers (i, j) with
i <= j, where neither i nor j is divisible by 2, 3, or 5,
and the pairs are ordered according to the sum 2i + 3j +
5ij.

|#

