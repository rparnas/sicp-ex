#include <string.h>
#include "scheme.h"

Sexp append(Sexp a, Sexp b)
{
  if (a.Type == Null)
  {
    return b;
  }
  return cons(GetPairCarValue(a), append(GetPairCdrValue(a), b));
}

Sexp booleanp(Sexp exp)
{
  return MakeBool(exp.Type == Bool);
}

Sexp caadr(Sexp s)
{
  return car(car(cdr(s)));
}

Sexp cadddr(Sexp s)
{
  return car(cdr(cdr(cdr(s))));
}

Sexp caddr(Sexp s)
{
  return car(cdr(cdr(s)));
}

Sexp cadr(Sexp s)
{
  return car(cdr(s));
}

Sexp car(Sexp s)
{
  return GetPairCarValue(s);
}

Sexp cdadr(Sexp s)
{
  return cdr(car(cdr(s)));
}

Sexp cddr(Sexp s)
{
  return cdr(cdr(s));
}

Sexp cdr(Sexp s)
{
  return GetPairCdrValue(s);
}

Sexp cons(Sexp a, Sexp b)
{
  return MakePair(a, b);
}

Sexp error(char* message)
{
  return MakeError(message);
}

Sexp eqp(Sexp a, Sexp b)
{
  return eqvp(a, b); // TODO: Not sure if this is compliant or not.
}

/*
  The eqv? procedure defines a useful equivalence relation on objects. Briefly,
  it returns #t if obj1 and obj2 should normally be regarded as the same object. 
  This relation is left slightly open to interpretation, but the following 
  partial specification of eqv? holds for all implementations of Scheme.

  The eqv? procedure returns #t if:

  * obj1 and obj2 are both #t or both #f.
  * obj1 and obj2 are both symbols and (string=? (symbol->string obj1) (symbol->string obj2)) ===> #t
  * obj1 and obj2 are both characters and are the same character according to the char=? procedure (section 6.3.4).
  * both obj1 and obj2 are the empty list.
  * obj1 and obj2 are pairs, vectors, or strings that denote the same locations in the store (section 3.4).
  * obj1 and obj2 are procedures whose location tags are equal (section 4.1.4).

  The eqv? procedure returns #f if:

  * obj1 and obj2 are of different types (section 3.2).
  * one of obj1 and obj2 is #t but the other is #f.
  * obj1 and obj2 are symbols but (string=? (symbol->string obj1) (symbol->string obj2)) ===>  #f
  * one of obj1 and obj2 is an exact number but the other is an inexact number.
  * obj1 and obj2 are numbers for which the = procedure returns #f.
  * obj1 and obj2 are characters for which the char=? procedure returns #f.
  * one of obj1 and obj2 is the empty list but the other is not.
  * obj1 and obj2 are pairs, vectors, or strings that denote distinct locations.
  * (obj1 and obj2 are procedures that would behave differently (return different value(s) or have different side effects) for some arguments.

*/
Sexp eqvp(Sexp a, Sexp b)
{
  if (a.Type != b.Type)
  {
    return MakeBool(0);
  }

  if (a.Type == Bool)
  {
    return MakeBool(GetBoolValue(a) == GetBoolValue(b));
  }

  if (a.Type == Symbol)
  {
    return string_equalsp(symbol_to_string(a), symbol_to_string(b));
  }

  if (a.Type == Number && b.Type == Number)
  {
    return number_equals(a, b);
  }

  // TODO: Characters

  if (a.Type == Null && b.Type == Null)
  {
    return MakeBool(1);
  }
  
  if (a.Type == Pair && b.Type == Pair)
  {
    return MakeBool(AreSamePairInStore(a, b));
  }

  // TODO: vectors

  if (a.Type == String && b.Type == String)
  {
    // TODO: strings -- needs to be updated if strings are made to match the scheme spec
    return string_equalsp(a, b);
  }

  // TODO: procedures

  return MakeBool(0);
}

/*
  Equal? recursively compares the contents of pairs, vectors, and strings,
  applying eqv? on other objects such as numbers and symbols. A rule of thumb 
  is that objects are generally equal? if they print the same. Equal? may fail 
  to terminate if its arguments are circular data structures.
*/
Sexp equalp(Sexp a, Sexp b)
{
  //printf("\n");
  //Debug(a, 0);

  //printf("\n");
  //Debug(b, 0);

  if (a.Type == Pair && b.Type == Pair)
  {
    return SexpAnd2(equalp(GetPairCarValue(a), GetPairCarValue(b)),
                    equalp(GetPairCdrValue(a), GetPairCdrValue(b)));
  }

  // TODO: vectors

  if (a.Type == String && b.Type == String)
  {
    return string_equalsp(a, b);
  }

  return eqvp(a, b);
}

Sexp length(Sexp ls)
{
  int count = 0;

  while (ls.Type != Null)
  {
    ls = cdr(ls);
  }

  return MakeNumber(count);
}

Sexp list1(Sexp s1)
{
  return 
    cons(s1, MakeNull());
}

Sexp list2(Sexp s1, Sexp s2)
{
  return 
    cons(s1,
      cons(s2, MakeNull()));
}

Sexp list3(Sexp s1, Sexp s2, Sexp s3)
{
  return 
    cons(s1,
      cons(s2,
        cons(s3, MakeNull())));
}

Sexp list4(Sexp s1, Sexp s2, Sexp s3, Sexp s4)
{
  return
    cons(s1,
      cons(s2,
        cons(s3,
          cons(s4, MakeNull()))));
}

Sexp nullp(Sexp a)
{
  return MakeBool(a.Type == Null);
}

Sexp number_equals(Sexp a, Sexp b)
{
  if (a.Type != Number || b.Type != Number)
  {
    return MakeBool(0); // TODO: Error
  }
  return MakeBool(GetNumberValue(a) == GetNumberValue(b));
}

Sexp number_minus(Sexp a, Sexp b)
{
  return MakeNumber(GetNumberValue(a) - GetNumberValue(b));
}

Sexp number_multiply(Sexp a, Sexp b)
{
  return MakeNumber(GetNumberValue(a) * GetNumberValue(b));
}

Sexp number_lt(Sexp a, Sexp b)
{
  int numa = GetNumberValue(a);
  int numb = GetNumberValue(b);
  return MakeBool(numa < numb);
}

Sexp numberp(Sexp s)
{
  return MakeBool(s.Type == Number);
}

Sexp pairp(Sexp s)
{
  return MakeBool(s.Type == Pair);
}

void set_car_bang(Sexp a, Sexp new_car)
{
  SetCar(a, new_car);
}

void set_cdr_bang(Sexp a, Sexp new_cdr)
{
  SetCdr(a, new_cdr);
}

Sexp string_equalsp(Sexp a, Sexp b)
{
  if (a.Type != String || b.Type != String)
  {
    return MakeBool(0); // TODO: Error
  }
  
  return MakeBool(strcmp(GetStringValue(a), GetStringValue(b)) == 0);
}

Sexp stringp(Sexp s)
{
  return MakeBool(s.Type == String);
}

Sexp symbolp(Sexp s)
{
  return MakeBool(s.Type == Symbol);
}

Sexp symbol_to_string(Sexp s)
{
  return MakeString(GetStringValue(s));
}
