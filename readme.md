# sicp-ex
This provides resources for completing the exercises from *Structure and Interpretation of Computer Programs*:

- Pre-made exercise files with the prompt of each exercise as a plaintext scheme comment.
- Ability to jump between exercises without moving through commit history. This includes resetting the environment to the context of any given exercise.
- Automated testing and regression testing.

Currently, this has been tried with [Chez Scheme](https://github.com/cisco/ChezScheme) and [Racket](https://github.com/racket/racket) on Windows.

Exercise prompts were parsed from a fork of [sarabander/sicp-pdf](https://github.com/sarabander/sicp-pdf) at [this commit](https://github.com/rparnas/sicp-pdf/commit/62d2b424470f37587f056ab73addc850a7d6da8e).

# Instructions
1. Checkout this repository, `sicp-ex`.
2. Write your answers in the exercise files at `\exercises\`.
3. Start your interpreter using the bash script `ex` in the repository root, passing the name of your interpreter like `.\ex scheme` or `.\ex racket`. This makes the utilities in `ex.ss` available to every exercise. Feel free to add your own.
4. When an answer needs the contents of a previous answer invoke `load-ex` in your exercise file like `(load-ex "2.82")`.
5. Add tests to your exercise by using the `define-test` syntax which takes an expression and an expected result like `(define-test (+ 2 2) 4)`. These tests are run whenever you load an exercise.
6. If some tests fail, invoke `(run-tests)` from the REPL to see the details.
7. To load an exercise and also reset your interpreter completely invoke `reset-ex` in the REPL like  `(reset-ex "2.83")`.
8. See the full list of utilities available below.

# Example

**.\exercises\6.1.ss**

````
#|

Exercise 6.1: Define a function that multiplies a number by seven. 

|#

#| Answer |#
(define (times-seven x) (* 7 x))

#| Tests |#
(define-test (times-seven 5) 35)
(define-test (times-seven 6) 42)
````

**.\exercises\6.2.ss**

````
#| 

Exercise 6.2: Using the results of the previous exercise, define a function
that multiplies odd numbers by seven and even numbers by 2.

|#

#| Answer |#
(load-ex "6.1")

(define (times-two x) (* 2 x))

(define (func x) 
  (if (odd? x)
      (times-seven x)
      (times-two x)))
      
#| Tests -- infrastructure |#
(define-test (times-two 2) 4)
(define-test (times-two 4) 8)

#| Tests |#
(define-test (func 5) 35)
(define-test (func 6) 12)
````

**Execution**

````
User@PC ~/Projects/sicp-ex
$ ./ex scheme
Chez Scheme Version 9.5
Copyright 1984-2017 Cisco Systems, Inc.

> (reset-ex "6.2")
Chez Scheme Version 9.5
Copyright 1984-2017 Cisco Systems, Inc.

Loaded .\exercises\6.1.ss (2/2 tests passed)
Loaded .\exercises\6.2.ss (4/4 tests passed, 2/2 regression tests passed)
````

# Utilities

### dec
`(dec n)` Decrements the given number by 1.

````
> (dec 1)
0
````

### define-test

`(define-test (+ 2 2) 4)` Defines an automated test. Tests are run whenever an exercise file is loaded. If an exercise answer loads a previous answer via `load-ex` the tests for that previous answer are run in the context of the new exercise, to make sure that the new answer hasn't broken previous functionality. 

### get-failing-tests
`(reset-ex exercise)` Same as `run-tests` except only displays the results of
failing tests.

### inc

`(inc n)` Increments the given number by 1.

````
> (inc 1)
2
````

### load-ex
`(load-ex exercise)` Given the name of an exercise like "2.7" loads the corresponding exercise file and runs any automated tests.

````
; An exercise with two tests.
> (load-ex "2.7")
Loaded .\exercises\2.7.ss (2/2 tests passed)
````

```
; An exercise with one test that depends on a previous exercise.
> (load-ex "2.8")
Loaded .\exercises\2.7.ss (2/2 tests passed)
Loaded .\exercises\2.8.ss (1/1 test passed, 2/2 regression tests passed)
```

### reset-ex

`(reset-ex exercise)` Given the name of an exercise like "2.7", exits and reopens the interpreter and then loads the corresponding exercise file and runs any automated tests.

````
> (reset-ex "2.8")
Welcome to Scheme!

Loaded .\exercises\2.7.ss (2/2 tests passed)
Loaded .\exercises\2.8.ss (1/1 test passed, 2/2 regression tests passed)
````

### run-tests

`(run-tests)` Runs tests for the currently loaded exercise and returns the details of what has passed and failed.

### square

`(square n)`
Squares the given number.

````
> (square 2)
4
````

### tracize
`(traceize f)`
Returns a modified version of the given function which traces itself by displaying a line of text each time that function is invoked.

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
| Lecture | Title | Text | YouTube |
| - | - | - | - |
| 1A | Overview: Introduction to Lisp | 1.1 |  [Link](https://www.youtube.com/watch?v=2Op3QLzMgSY) |
| 1B | Procedures & Processes: Substitution Model | 1.2 |  [Link](https://www.youtube.com/watch?v=dlbMuv-jix8) |
| 2A | Higher-order Procedures | 1.3 |  [Link](https://www.youtube.com/watch?v=erHp3r6PbJk) |
| 2B | Compound Data | 2.1 <br>2.2 (?) |  [Link](https://www.youtube.com/watch?v=ymsbTVLbyN4) |
| 3A | Henderson Escher Example | 2.2.4 (?) |  [Link](https://www.youtube.com/watch?v=2QgZVYI3tDs) |
| 3B | Symbolic Differentiation: Quotation | 2.2 <br>2.3.1 (?) <br>2.3.2 (?) |  [Link](https://www.youtube.com/watch?v=X21cKVtGvYk) |
| 4A | Pattern-matching: Rule-based Substitution | Data-driven Programming (?) |  [Link](https://www.youtube.com/watch?v=amf5lTZ0UTc) |
| 4B | Generic Operators | 2.3 <br>2.4 <br>2.5 |  [Link](https://www.youtube.com/watch?v=h6Z7vx9iUB8) |
| 5A | Assignment, State, & Side-effects | 3.1 <br>3.2 |  [Link](https://www.youtube.com/watch?v=jl8EHP1WrWY) |
| 5B | Conditional Objects | 3.2 (?) <br> 3.3 <br>3.4 (?) |  [Link](https://www.youtube.com/watch?v=SsBxcpkyMMw) |
| 6A | Streams I | 3.4 <br>3.5 (?) |  [Link](https://www.youtube.com/watch?v=a2Qt9uxhNSM) |
| 6B | Streams | 3.4 <br>3.5 (?) |  [Link](https://www.youtube.com/watch?v=DCub3iqteuI) |
| 7A | Metacircular Evaluator I | 4.1 |  [Link](https://www.youtube.com/watch?v=0m6hoOelZH8) |
| 7B | Metacircular Evaluator II | 4.2 |  [Link](https://www.youtube.com/watch?v=t5EI5fXX8K0) |
| 8A | Logic Programming I | 4.4 <br>4.5 (?) <br> |  [Link](https://www.youtube.com/watch?v=cyVXjnFL2Ps) |
| 8B | Logic Programming II | 4.4 <br>4.5 (?) <br> |  [Link](https://www.youtube.com/watch?v=R3uRidfSpc4) |
| 9A | Register Machines | 5.1 <br>5.2 (?) |  [Link](https://www.youtube.com/watch?v=jPDAPmx4pXE) |
| 9B | Explicit-control Evaluator | 5.2 <br>5.4 (?) |  [Link](https://www.youtube.com/watch?v=SLcZXbyGC3E) |
| 10A | Compilation | 5.3 <br>5.5 (?) |  [Link](https://www.youtube.com/watch?v=kNmiTTKiYd4) |
| 10B | Garbage Collection | 5.3 (?) <br>5.4 |  [Link](https://www.youtube.com/watch?v=2s2_FAf-yQs) |

# Future Ideas
* Exercise prompt fixes
  * 2.13 -- too much text
  * 2.42 and 3.73 -- figures are labeled twice
  * 3.8 -- missing a diagram.
  * 3.30 -- missing a figure of a ripple-carry adder
  * 4.55 -- is labeled a/b/c instead of 1/2/3
  * Reference page numbers (?)
  * Document the expected dependencies between answers (?). This breaks down as the exercises become more advanced you have more options as to whether you want to proceed via a patch to a previous exercise or a copy-paste.
  * Include code "copied from book" needed for answers.
  * Include a packet of domain review like a half-page review for 
  * Include exercise hints, especially for any exercises that tend to get people stuck on dead-ends that prompt didn't intend for you to explore.
* Resources
  * Verify lecture to text mapping.
* Testing
  * floating point expected results.
  * better handling of answers which should break compatibility with previous exercises.
* Utilities
  * Would be nice to eliminate bash script.

# License
This is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License ([cc by-sa](http://creativecommons.org/licenses/by-sa/4.0/)) as this is the license of document from which the exercise prompts were extracted.
