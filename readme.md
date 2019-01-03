SICP Exercises
--------------

This project contains starter files for filling in to complete the exercises from *Structures and Interperetation of Computer Programs, 2nd Edition*. Exercise prompts were parsed from a fork of [sarabander/sicp-pdf](https://github.com/sarabander/sicp-pdf) at [this commit](https://github.com/rparnas/sicp-pdf/commit/62d2b424470f37587f056ab73addc850a7d6da8e).

Also included are some utilities. Execute the script `ex` to use them. Invoking the function `ex` such as `(ex "1.14")` will exit and re-open scheme clearing the global environment and loading the appropriate exercise file.

Features
--------
* Exercise prompts as plaintext scheme comments.
* Function to quickly reload exercise files and clear the global environment.

Issues
-----
* Exercises 2.42 and 3.73 are labeled twice.
* Exercise 3.8 is missing a diagram.
* Exercise 3.30 is missing a figure of a ripple-carry adder.
* Exercise 4.55: is labeled a/b/c instead of 1/2/3.

Ideas
--------
* Reference where in the physical book exercises are located.
* Include code from the book that exercises rely on.
* Highlight dependency between multiple exercises.
* Some exercises involve iteratively improving something. Could a scaffold be provided that allows quickly switching between multiple versions of the same thing in a clear manner? For example: how can one avoid the confusion/tedium of knowing which version of a function is currently referred to in the global environment?
* Improve quick reload script.

Lectures
--------
| Lecture | Title                                      | YouTube                                             |
| ------- | ------------------------------------------ |---------------------------------------------------- |
| 1A      | Overview: Introduction to Lisp             | [Link](https://www.youtube.com/watch?v=2Op3QLzMgSY) |
| 1B      | Procedures & Processes: Substitution Model | [Link](https://www.youtube.com/watch?v=dlbMuv-jix8) |
| 2A      | Higher-order Procedures                    | [Link](https://www.youtube.com/watch?v=erHp3r6PbJk) |
| 2B      | Compound Data                              | [Link](https://www.youtube.com/watch?v=ymsbTVLbyN4) |
| 3A      | Henderson Escher Example                   | [Link](https://www.youtube.com/watch?v=2QgZVYI3tDs) |
| 3B      | Symbolic Differentiation: Quotation        | [Link](https://www.youtube.com/watch?v=X21cKVtGvYk) |
| 4A      | Pattern-matching: Rule-based Substitution  | [Link](https://www.youtube.com/watch?v=amf5lTZ0UTc) |
| 4B      | Generic Operators                          | [Link](https://www.youtube.com/watch?v=h6Z7vx9iUB8) |
| 5A      | Assignment, State, & Side-effects          | [Link](https://www.youtube.com/watch?v=jl8EHP1WrWY) |
| 5B      | Conditional Objects                        | [Link](https://www.youtube.com/watch?v=SsBxcpkyMMw) |
| 6A      | Streams I                                  | [Link](https://www.youtube.com/watch?v=a2Qt9uxhNSM) |
| 6B      | Streams                                    | [Link](https://www.youtube.com/watch?v=DCub3iqteuI) |
| 7A      | Metacircular Evaluator I                   | [Link](https://www.youtube.com/watch?v=0m6hoOelZH8) |
| 7B      | Metacircular Evaluator II                  | [Link](https://www.youtube.com/watch?v=t5EI5fXX8K0) |
| 8A      | Logic Programming I                        | [Link](https://www.youtube.com/watch?v=cyVXjnFL2Ps) |
| 8B      | Logic Programming II                       | [Link](https://www.youtube.com/watch?v=R3uRidfSpc4) |
| 9A      | Register Machines                          | [Link](https://www.youtube.com/watch?v=jPDAPmx4pXE) |
| 9B      | Explicit-control Evaluator                 | [Link](https://www.youtube.com/watch?v=SLcZXbyGC3E) |
| 10A     | Compilation                                | [Link](https://www.youtube.com/watch?v=kNmiTTKiYd4) |
| 10B     | Garbage Collection                         | [Link](https://www.youtube.com/watch?v=2s2_FAf-yQs) |

License
-------
This is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License ([cc by-sa](http://creativecommons.org/licenses/by-sa/4.0/)) as this is the license of document from which the exercise prompts were extracted.
