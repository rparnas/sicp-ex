#|

Exercise 3.44: Consider the problem of transferring an
amount from one account to another. Ben Bitdiddle claims
that this can be accomplished with the following procedure,
even if there are multiple people concurrently transferring
money among multiple accounts, using any account mechanism
that serializes deposit and withdrawal transactions, for
example, the version of "make-account" in the text above.

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

Louis Reasoner claims that there is a problem here, and that
we need to use a more sophisticated method, such as the one
required for dealing with the exchange problem. Is Louis
right? If not, what is the essential difference between the
transfer problem and the exchange problem? (You should
assume that the balance in "from-account" is at least
"amount".)

|#

#| Answer 

There isn't a problem. If there isn't enough money to withdraw the transfer
fails. If there is enough money the withdraw happens and eventually the deposit
happens. Values are never left in an invalid state.

The difference is that the amount is arbitrary and provided by the user. For
exchange, the values of the operations are coupled to the relative value of the
two accounts.

Consider that the spec for exchange is "at the end, the accounts should have
swapped values". Performing an operation that deposits or withdraws to those
values partway the exchange makes it ambiguous as to what the final result of
the "exchange" should be.

For example if A=10 and B=20, and at any arbitrary point throughout the exchange
A gets a deposit of 10, what should the final values be? The only way to answer
this question is to leak the implementation details of exchange into the spec or
to change it drastically to avoid concurrency problems in the first place.

On the other hand, "transfer x from a to b" has no coupling to the final value
of the accounts and no concurrency issues.

|#