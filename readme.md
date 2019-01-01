SICP Exercises
-------

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

License
-------
This is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License ([cc by-sa](http://creativecommons.org/licenses/by-sa/4.0/)) as this is the license of document from which the exercise prompts were extracted.
