#pragma once

#include "sexp.h"

#include <string.h>
#include "scheme.h"

Sexp append(Sexp a, Sexp b);
Sexp booleanp(Sexp exp);
Sexp caadr(Sexp s);
Sexp cadddr(Sexp s);
Sexp caddr(Sexp s);
Sexp cadr(Sexp s);
Sexp car(Sexp s);
Sexp cdadr(Sexp s);
Sexp cddr(Sexp s);
Sexp cdr(Sexp s);
Sexp cons(Sexp a, Sexp b);
Sexp error(char* message);
Sexp eqp(Sexp a, Sexp b);
Sexp eqvp(Sexp a, Sexp b);
Sexp equalp(Sexp a, Sexp b);
Sexp length(Sexp ls);
Sexp list1(Sexp s1);
Sexp list2(Sexp s1, Sexp s2);
Sexp list3(Sexp s1, Sexp s2, Sexp s3);
Sexp list4(Sexp s1, Sexp s2, Sexp s3, Sexp s4);
Sexp nullp(Sexp a);
Sexp number_equals(Sexp a, Sexp b);
Sexp number_lt(Sexp a, Sexp b);
Sexp number_multiply(Sexp a, Sexp b);
Sexp number_minus(Sexp a, Sexp b);
Sexp numberp(Sexp s);
Sexp pairp(Sexp s);
void set_car_bang(Sexp a, Sexp new_car);
void set_cdr_bang(Sexp a, Sexp new_cdr);
Sexp string_equalsp(Sexp a, Sexp b);
Sexp stringp(Sexp s);
Sexp symbolp(Sexp s);
Sexp symbol_to_string(Sexp s);
