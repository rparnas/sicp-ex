#|

Exercise 4.6: "let" expressions are derived expressions,
because

(let ((<var_(1)>  <exp_(1)> )   ...  (<var_(n)>  <exp_(n)> ))
  <body>)

is equivalent to

((lambda (<var_(1)> ... <var_(n)> )
   <body>)
 <exp_(1)> 
   ... 
 <exp_(n)> )

Implement a syntactic transformation "let->combination" that
reduces evaluating "let" expressions to evaluating
combinations of the type shown above, and add the
appropriate clause to "eval" to handle "let" expressions.

|#

