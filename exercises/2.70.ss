#|

Exercise 2.70: The following eight-symbol alphabet with
associated relative frequencies was designed to efficiently
encode the lyrics of 1950s rock songs. (Note that the
"symbols" of an "alphabet" need not be individual letters.)

A    2   GET 2   SHA 3   WAH 1
BOOM 1   JOB 2   NA 16   YIP 9

Use "generate-huffman-tree" (Exercise 2.69) to generate a
corresponding Huffman tree, and use "encode" (Exercise 2.68)
to encode the following message:

Get a job
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom

How many bits are required for the encoding? What is the
smallest number of bits that would be needed to encode this
song if we used a fixed-length code for the eight-symbol
alphabet?

|#

#| Answer 

84 bits are required.

A fixed-length encoding requires log_2(8) or 4 bits per word.

|#

(load-ex "2.69")

(define rock-alphabet
  '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define rock-tree
  (generate-huffman-tree rock-alphabet))

(define song
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

#| Tests |#
(define-test (length (encode song rock-tree)) 84)
