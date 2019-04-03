#|

Exercise 2.55: Eva Lu Ator types to the interpreter the
expression

(car "abracadabra)

To her surprise, the interpreter prints back "quote".
Explain.

|#

#| Answer

This is because (quote (quote abracadabra)) yields (quote
abracadabra) and you are taking the car of that.

|#