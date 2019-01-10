# SICP Exercises
This project intends to reduce the pain points of completing the exercises from *Structure and Interpretation of Computer Programs, 2nd Edition* by providing resources and utilities. This especially comes from the perspective of those who wish to work through the text incrementally or otherwise have the need to jump in and out quickly.

# Features
* One stub file per exercise including the exercise prompt as a plaintext scheme comment.
* Reset the global enviornment quickly.
* Ensure the global enviornment only has the context of the exercise being worked on.

# Instructions

Checkout this repository, `sicp-ex`. Write the answers to each exercise in its respective file under the `\exercises\` directory. Start your interpreter using the provided batch script `ex` in the repository root (i.e. execute `.\ex`). This starts your interpreter and gives you full access to the utilities provided in `ex.ss`. These utilities are functions meant to be available to every exercise. Feel free to add your own.

At the top of your exercise file you may load the contents of previous exercises using `load-ex` like `(load-ex "2.83")`. This displays the list of files loaded to help you keep track of the relationship between your exercise answers. It also avoids polluting the global environment with unecessary content.

To load a file and reset the global enviornment completely, run `load-ex` with an addition #t parameter like `(load-ex "2.83" #t)`. This allows you to effectively roll back to any exercise, even if its content is loaded and built on in subsequent exercises.

# Details
## Exercise Files
The exercise prompt in each file was parsed from a fork of [sarabander/sicp-pdf](https://github.com/sarabander/sicp-pdf) at [this commit](https://github.com/rparnas/sicp-pdf/commit/62d2b424470f37587f056ab73addc850a7d6da8e).

## Utilities
TBD

# Resources 
## Lectures
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

# Issues
* Issue: Exercises 2.42 and 3.73 figures are labeled twice.
* Issue: Exercise 3.8 is missing a diagram.
* Issue: Exercise 3.30 is missing a figure of a ripple-carry adder.
* Issue: Exercise 4.55: is labeled a/b/c instead of 1/2/3.
* Improvement: Reference physical page numbers
* Improvement: Include more utilit code.
* Improvement: Better document the expected dependencies between exercises
* Improvement: Make interpreter reload script work without having to use a batch script.
* Improvement: Uncomment the parts of exercises which require you to copy paste code from the prompt into the exercise answer.
* Improvement: Included "copied from book code" required for answers and which is not part of the exercise prompt.

# License
This is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License ([cc by-sa](http://creativecommons.org/licenses/by-sa/4.0/)) as this is the license of document from which the exercise prompts were extracted.
