#|

Exercise 3.36: Suppose we evaluate the following sequence of
expressions in the global environment:

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

At some time during evaluation of the "set-value!", the
following expression from the connector's local procedure is
evaluated:

(for-each-except
  setter inform-about-value constraints)

Draw an environment diagram showing the environment in which
the above expression is evaluated.

|#

#| Answer -- on paper |#

#| Notes
(set-value! connector:a new-value:10 informant:'user)
  (set-my-value newval:10 setter:'user)
    (define (set-my-value newval:10 setter:'user)
      (cond [(not (has-value? me)) => #t
             (set! value newval) => change value from #f to 10
             (set! informant setter) => change informant from #f to 'user
             (for-each-except setter
                              inform-about-value
                              constraints)]
            . . .
    (define (for-each-except exception procedure list)
            . . .

#|

