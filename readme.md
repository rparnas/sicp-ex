# SICP Exercises
This project intends to reduce the pain points of completing the exercises from *Structure and Interpretation of Computer Programs, 2nd Edition* by providing resources and utilities. This especially comes from the perspective of those who wish to work through the text incrementally or otherwise have the need to jump in and out quickly.

# Features
* One stub file per exercise including the exercise prompt as a plaintext scheme comment.
* Build exercises answers on one another while avoiding copy-pasting code. 
* Reset the global environment to the context of any single exercise without moving through the commit history.

# Instructions
1. Checkout this repository, `sicp-ex`.
2. Write answers to each exercise in the files at `\exercises\`.
3. Start your interpreter using the bash script `ex` in the repository root, passing the name of your interpreter like `.\ex scheme` or `.\ex racket`. This provides access to the utilities in `ex.ss` which are functions available to every exercise. Feel free to add your own.
4. In each exercise file, optionally load the contents of a previous exercise by adding `load-ex` to the source code of that file like `(load-ex "2.82")`.
5. Reset your interpreter to the context of any given exercise by invoking `load-ex` in the REPL with the additional parameter #t like `(load-ex "2.83" #t)`. This is useful for jumping forward or backward between exercises or for clearing the global environment of test expressions or traces.

# Details
## Exercise Files
The exercise prompt in each file was parsed from a fork of [sarabander/sicp-pdf](https://github.com/sarabander/sicp-pdf) at [this commit](https://github.com/rparnas/sicp-pdf/commit/62d2b424470f37587f056ab73addc850a7d6da8e).

## Utilities

### dec
`(dec n)`
Decrements the given number `n` by 1.

### inc
`(inc n)`
Increments the given number `n` by 1.

### load-ex
`(load-ex exercise-n)` or `(load-ex exercise-n should-reset)` Given the string name of an exercise (i.e. "1.7" or "2.52") loads the file associated with that exercise. If the optional #t argument is provided, it exits and restarts the interpreter before loading the file.

### square
`(square n)`
Squares the given number `n`.

### tracize
`(traceize func)`
Allows the given function to be traced by returning a version of it which will display the arguments with which that function was invoked. For example:
````
> (define (f n) (if (< n 1) n (f (- n 1))))
> (define f-original f)
> (set! f (tracize f))
> (f 4)
(#<procedure f> 4)
(#<procedure f> 3)
(#<procedure f> 2)
(#<procedure f> 1)
(#<procedure f> 0)
0
> (set! f f-original)
> (f 4)
0
````

# Resources 
## Lectures
| Lecture | Title | Text | YouTube |
| - | - | - | - |
| 1A | Overview: Introduction to Lisp | 1.1 |  [Link](https://www.youtube.com/watch?v=2Op3QLzMgSY) |
| 1B | Procedures & Processes: Substitution Model | 1.2 |  [Link](https://www.youtube.com/watch?v=dlbMuv-jix8) |
| 2A | Higher-order Procedures | 1.3 |  [Link](https://www.youtube.com/watch?v=erHp3r6PbJk) |
| 2B | Compound Data | 2.1 <br> 2.2 (?) |  [Link](https://www.youtube.com/watch?v=ymsbTVLbyN4) |
| 3A | Henderson Escher Example | 2.2.4 (?) |  [Link](https://www.youtube.com/watch?v=2QgZVYI3tDs) |
| 3B | Symbolic Differentiation: Quotation | 2.2 <br> 2.3.1 (?) <br> 2.3.2 (?) |  [Link](https://www.youtube.com/watch?v=X21cKVtGvYk) |
| 4A | Pattern-matching: Rule-based Substitution | Data-driven Programming (?) |  [Link](https://www.youtube.com/watch?v=amf5lTZ0UTc) |
| 4B | Generic Operators | 2.3 <br> 2.4 <br> 2.5 |  [Link](https://www.youtube.com/watch?v=h6Z7vx9iUB8) |
| 5A | Assignment, State, & Side-effects | 3.1 <br> 3.2 |  [Link](https://www.youtube.com/watch?v=jl8EHP1WrWY) |
| 5B | Conditional Objects | 3.2 (?) <br> 3.3 <br> 3.4 (?) |  [Link](https://www.youtube.com/watch?v=SsBxcpkyMMw) |
| 6A | Streams I | 3.4 <br> 3.5 (?) |  [Link](https://www.youtube.com/watch?v=a2Qt9uxhNSM) |
| 6B | Streams | 3.4 <br> 3.5 (?) |  [Link](https://www.youtube.com/watch?v=DCub3iqteuI) |
| 7A | Metacircular Evaluator I | 4.1 |  [Link](https://www.youtube.com/watch?v=0m6hoOelZH8) |
| 7B | Metacircular Evaluator II | 4.2 |  [Link](https://www.youtube.com/watch?v=t5EI5fXX8K0) |
| 8A | Logic Programming I | 4.4 <br> 4.5 (?) <br> |  [Link](https://www.youtube.com/watch?v=cyVXjnFL2Ps) |
| 8B | Logic Programming II | 4.4 <br> 4.5 (?) <br> |  [Link](https://www.youtube.com/watch?v=R3uRidfSpc4) |
| 9A | Register Machines | 5.1 <br> 5.2 (?) |  [Link](https://www.youtube.com/watch?v=jPDAPmx4pXE) |
| 9B | Explicit-control Evaluator | 5.2 <br> 5.4 (?) |  [Link](https://www.youtube.com/watch?v=SLcZXbyGC3E) |
| 10A | Compilation | 5.3 <br> 5.5 (?) |  [Link](https://www.youtube.com/watch?v=kNmiTTKiYd4) |
| 10B | Garbage Collection | 5.3 (?) <br> 5.4 |  [Link](https://www.youtube.com/watch?v=2s2_FAf-yQs) |

# Issues
* Issue: Exercises 2.42 and 3.73 figures are labeled twice.
* Issue: Exercise 3.8 is missing a diagram.
* Issue: Exercise 3.30 is missing a figure of a ripple-carry adder.
* Issue: Exercise 4.55: is labeled a/b/c instead of 1/2/3.
* Issue: Verify lecture to text mapping
* Improvement: Reference physical page numbers
* Improvement: Include more utilit code.
* Improvement: Better document the expected dependencies between exercises
* Improvement: Make interpreter reload script work without having to use a bash script.
* Improvement: Uncomment parts of prompt with code needed for the answer.
* Improvement: Included "copied from book code" required for answers and which is not part of the exercise prompt.

# License
This is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License ([cc by-sa](http://creativecommons.org/licenses/by-sa/4.0/)) as this is the license of document from which the exercise prompts were extracted.
