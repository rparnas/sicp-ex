#|

Exercise 3.32: The procedures to be run during each time
segment of the agenda are kept in a queue. Thus, the
procedures for each segment are called in the order in which
they were added to the agenda (first in, first out). Explain
why this order must be used. In particular, trace the
behavior of an and-gate whose inputs change from 0, 1 to 1,
0 in the same segment and say how the behavior would differ
if we stored a segment's procedures in an ordinary list,
adding and removing procedures only at the front (last in,
first out).

|#

#| Answer 

The actions must be processed FIFO because when two things happen simultaneously
only the action queued by the last thing the system was informed has the correct
value for output.

;;; Setup
  ; initial state
  ; a=0, b=1, output=0
  ; queue = ()

  (set-signal! A 1)
  ; a=1, b=1, output=0
  ; queue = ((3 (lambda () (set-signal! output 1)))

  (set-signal! B 0)
  ; a=1, b=0, output=0
  ; queue = ((3 (lambda () (set-signal! output 1))
                (lambda () (set-signal! output 0)))

;;; FIFO
  ; first item in queue processed
  ; a=1, b=0, output=1
  ; queue = ((3 (lambda () (set-signal! output 0)))

  ; remaining item in queue processed
  ; a=1, b=0, output=0
  ; queue = ()

;;; LIFO
  ; last item in queue processed
  ; a=1, b=0, output=0
  ; queue = ((3 (lambda () (set-signal! output 1)))

  ; remaining item in queue processed -- incorrect answer
  ; a=1, b=0, output=1
  ; queue = ()

|#

#| Notes

Perhaps the system should be designed more robustly by checking what the current
value of A and B are before setting output.

Or if the action system was restricted to only "setting a wire to 0 or 1"
it could be represented like ((3 (output . 0)) and could be smart enough
to collapse redundant or stale actions. 

|#
