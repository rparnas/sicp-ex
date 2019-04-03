#pragma once

#include "evaluator.h"
#include "parser.h"
#include "scheme.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* 4.1: Code from book -- hacks */

/* 4.1: Code from book -- self-evaluating */

Sexp self_evaluatingp(Sexp exp)
{
  // modified -- booleans are self-evaluating rather than in the environment.
  return 
    SexpOr2(booleanp(exp), 
      SexpOr2(numberp(exp), stringp(exp)));
}

/* 4.1: Code from book -- variable */

Sexp variablep(Sexp exp)
{
  return symbolp(exp);
}

/* 4.1: Code from book -- quoted */

Sexp quotedp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("quote"));
}

Sexp text_of_quotation(Sexp exp)
{
  return cadr(exp);
}

/* 4.1: Code from book -- assignment */

Sexp assignmentp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("set!"));
}

Sexp assignment_variable(Sexp exp)
{
  return cadr(exp);
}

Sexp assignment_value(Sexp exp)
{
  return caddr(exp);
}

/* 4.1: Code from book -- definition */

Sexp definitionp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("define"));
}

Sexp definition_variable(Sexp exp)
{
  if (GetBoolValue(symbolp(cadr(exp))))
  {
    return cadr(exp);
  }
  return caadr(exp);
}

Sexp definition_value(Sexp exp)
{
  if (GetBoolValue(symbolp(cadr(exp))))
  {
    return caddr(exp);
  }
  return make_lambda(cdadr(exp), // formal parameters
                     cddr(exp)); // body              
}

/* 4.1: Code from book -- if */

Sexp make_if(Sexp predicate, Sexp consequent, Sexp alternative)
{
  return list4(MakeSymbol("if"), predicate, consequent, alternative);
}

Sexp ifp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("if"));
}

Sexp if_predicate(Sexp exp)
{
  return cadr(exp);
}

Sexp if_consequent(Sexp exp)
{
  return caddr(exp);
}

Sexp if_alternative(Sexp exp)
{
  if (!GetBoolValue(nullp(cddr(exp))))
  {
    return cadddr(exp);
  }
  return MakeSymbol("false");
}

/* 4.1: Code from book -- lambda */
Sexp make_lambda(Sexp parameters, Sexp body)
{
  return cons(MakeSymbol("lambda"), cons(parameters, body));
}

Sexp lambdap(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("lambda"));
}

Sexp lambda_parameters(Sexp exp)
{
  return cadr(exp);
}

Sexp lambda_body(Sexp exp)
{
  return cddr(exp);
}

Sexp make_procedure(Sexp parameters, Sexp body, Sexp env)
{
  return list4(MakeSymbol("procedure"), parameters, body, env);
}

Sexp compound_procedurep(Sexp p)
{
  return tagged_listp(p, MakeSymbol("procedure"));
}

Sexp procedure_parameters(Sexp p)
{
  return cadr(p);
}

Sexp procedure_body(Sexp p)
{
  return caddr(p);
}

Sexp procedure_environment(Sexp p)
{
  return cadddr(p);
}

/* 4.1: Code from book -- begin */

Sexp beginp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("begin"));
}

Sexp begin_actions(Sexp seq)
{
  return cdr(seq);
}

Sexp last_expp(Sexp seq)
{
  return nullp(cdr(seq));
}

Sexp first_exp(Sexp seq)
{
  return car(seq);
}

Sexp rest_exps(Sexp seq)
{
  return cdr(seq);
}

Sexp sequence_to_exp(Sexp seq)
{
  if (GetBoolValue(nullp(seq)))
  {
    return seq;
  }
  if (GetBoolValue(last_expp(seq)))
  {
    return first_exp(seq);
  }
  return make_begin(seq);
}

Sexp make_begin(Sexp seq)
{
  return cons(MakeSymbol("begin"), seq);
}

/* 4.1: Code from book -- cond */

Sexp condp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("cond"));
}

Sexp cond_clauses(Sexp exp)
{
  return cdr(exp);
}

Sexp cond_else_clausep(Sexp exp)
{
  return eqp(cond_predicate(exp), MakeSymbol("else"));
}

Sexp cond_predicate(Sexp exp)
{
  return car(exp);
}

Sexp cond_actions(Sexp exp)
{
  return cdr(exp);
}

Sexp cond_to_if(Sexp exp)
{
  return expand_clauses(cond_clauses(exp));
}

Sexp expand_clauses(Sexp clauses)
{
  if (GetBoolValue(nullp(clauses)))
  {
    return MakeSymbol("false");
  }

  Sexp first = car(clauses);
  Sexp rest = cdr(clauses);

  if (GetBoolValue(cond_else_clausep(first)))
  {
    if (GetBoolValue(nullp(rest)))
    {
      sequence_to_exp(cond_actions(first));
    }
    else
    {
      return MakeError("cond -- else clause isn't last");
    }
  }

  return make_if(cond_predicate(first),
                 sequence_to_exp(cond_actions(first)),
                 expand_clauses(rest));
}

/* 4.1: Code from book -- application */

Sexp applicationp(Sexp exp)
{
  return pairp(exp);
}

Sexp operator(Sexp exp)
{
  return car(exp);
}

Sexp operands(Sexp exp)
{
  return cdr(exp);
}

Sexp no_operandsp(Sexp ops)
{
  return nullp(ops);
}

Sexp first_operand(Sexp ops)
{
  return car(ops);
}

Sexp rest_operands(Sexp ops)
{
  return cdr(ops);
}

/* 4.1: Code from book -- utilities */

Sexp tagged_listp(Sexp exp, Sexp tag)
{
  if (GetBoolValue(pairp(exp)))
  {
    return eqp(car(exp), tag);
  }

  return MakeBool(0);
}

/* 4.1: Code from book -- environments */

Sexp enclosing_environment(Sexp env)
{
  return cdr(env);
}

Sexp first_frame(Sexp env)
{
  return car(env);
}

Sexp the_empty_environment()
{
  return MakeNull();
}

Sexp make_frame(Sexp variables, Sexp values)
{
  return cons(variables, values);
}

Sexp frame_variables(Sexp frame)
{
  return car(frame);
}

Sexp frame_values(Sexp frame)
{
  return cdr(frame);
}

void add_binding_to_frame_bang(Sexp var, Sexp val, Sexp frame)
{
  set_car_bang(frame, cons(var, car(frame)));
  set_cdr_bang(frame, cons(val, cdr(frame)));
}

Sexp extend_environment(Sexp vars, Sexp vals, Sexp base_env)
{
  if (GetBoolValue(number_equals(length(vars), length(vals))))
  {
    return cons(make_frame(vars, vals), base_env);
  }
  if (GetBoolValue(number_lt(length(vars), length(vals))))
  {
    return error("extend-environment -- too many arguments");
  }
  return error("extend-environment -- too few arguments");
}

static Sexp luscan(Sexp var, Sexp env, Sexp vars, Sexp vals)
{
  if (GetBoolValue(nullp(vars)))
  {
    return luenv_loop(var, enclosing_environment(env));
  }
  if (GetBoolValue(eqp(var, car(vars))))
  {
    return car(vals);
  }
  return luscan(var, env, cdr(vars), cdr(vals));
}

static Sexp luenv_loop(Sexp var, Sexp env)
{
  if (GetBoolValue(eqp(env, the_empty_environment())))
  {
    char buffer[200];
    sprintf_s(buffer, sizeof(buffer), "unbound variable -- %s", GetSymbolValue(var));
    return error(buffer);
  }

  Sexp frame = first_frame(env);
  return luscan(var, env,
                frame_variables(frame),
                frame_values(frame));
}

Sexp lookup_variable_value(Sexp var, Sexp env)
{
  return luenv_loop(var, env);
}

static void sscan(Sexp var, Sexp val, Sexp env, Sexp vars, Sexp vals)
{
  if (GetBoolValue(nullp(vars)))
  {
    senv_loop(var, val, enclosing_environment(env));
  }
  else if (GetBoolValue(eqp(var, car(vars))))
  {
    set_car_bang(vals, val);
  }
  else
  {
    sscan(var, val, env, cdr(vars), cdr(vals));
  }
}

static void senv_loop(Sexp var, Sexp val, Sexp env)
{
  if (GetBoolValue(eqp(env, the_empty_environment())))
  {
    return; // TODO -- error("set! -- unbound variable");
  }

  Sexp frame = first_frame(env);
  sscan(var, val, env,
        frame_variables(frame),
        frame_values(frame));
}

void set_variable_value_bang(Sexp var, Sexp val, Sexp env)
{
  senv_loop(var, val, env);
}

static void dscan(Sexp var, Sexp val, Sexp frame, Sexp vars, Sexp vals)
{
  if (GetBoolValue(nullp(vars)))
  {
    add_binding_to_frame_bang(var, val, frame);
  }
  else if (GetBoolValue(eqp(var, car(vars))))
  {
    set_car_bang(vals, var);
  }
  else
  {
    dscan(var, val, frame, cdr(vars), cdr(vals));
  }
}

void define_variable_bang(Sexp var, Sexp val, Sexp env)
{
  Sexp frame = first_frame(env);
  dscan(var, val, frame, frame_variables(frame), frame_values(frame));
}

/* 4.1: primitive procedures */
Sexp primitive_procedurep(Sexp proc)
{
  return tagged_listp(proc, MakeSymbol("primitive"));
}

Sexp primitive_implementation(Sexp proc)
{
  return cadr(proc);
}

const Sexp apply_zero_or_more_arg_procedure(char* pName, Sexp args)
{
  if (!strcmp(pName, "list"))
  {
    if (GetBoolValue(nullp(args)))
    {
      return MakeNull();
    }

    Sexp head = MakePair(car(args), MakeNull());
    Sexp tail = head;
    args = cdr(args);

    while (!GetBoolValue(nullp(args)))
    {
      Sexp newTail = MakePair(car(args), MakeNull());
      SetCdr(tail, newTail);
      tail = newTail;

      args = cdr(args);
    }

    return head;
  }
  if (!strcmp(pName, "+"))
  {
    int ret = 0;
    while (!GetBoolValue(nullp(args)))
    {
      ret += GetNumberValue(car(args));
      args = cdr(args);
    }
    return MakeNumber(ret);
  }
  if (!strcmp(pName, "*"))
  {
    if (GetBoolValue(nullp(args)))
    {
      return MakeNumber(1);
    }

    int ret = GetNumberValue(car(args));
    args = cdr(args);

    while (!GetBoolValue(nullp(args)))
    {
      ret *= GetNumberValue(car(args));
      args = cdr(args);
    }
    return MakeNumber(ret);
  }
}

const Sexp apply_one_or_more_arg_procedure(char* pName, Sexp args)
{
  if (GetBoolValue(nullp(args)))
  {
    return MakeError("expected at least 1 argument");
  }

  Sexp one = car(args);
  Sexp rest = cdr(args);

  if (!strcmp(pName, "-"))
  {
    int num = GetNumberValue(one);

    if (GetBoolValue(nullp(one)))
    {
      return MakeNumber(-1 * num);
    }
    while (!GetBoolValue(nullp(rest)))
    {
      num -= GetNumberValue(car(rest));
      rest = cdr(rest);
    }
    return MakeBool(num);
  }
  if (!strcmp(pName, ">"))
  {
    int num = GetNumberValue(one);
    int ret = 1;
    while (!GetBoolValue(nullp(rest)))
    {
      if (num > (num = GetNumberValue(car(rest))))
      {
        rest = cdr(rest);
      }
      else
      {
        return MakeBool(0);
      }
    }
    return MakeBool(ret);
  }
  if (!strcmp(pName, ">="))
  {
    int num = GetNumberValue(one);
    int ret = 1;
    while (!GetBoolValue(nullp(rest)))
    {
      if (num >= (num = GetNumberValue(car(rest))))
      {
        rest = cdr(rest);
      }
      else
      {
        return MakeBool(0);
      }
    }
    return MakeBool(ret);
  }
  if (!strcmp(pName, "<"))
  {
    int num = GetNumberValue(one);
    int ret = 1;
    while (!GetBoolValue(nullp(rest)))
    {
      if (num < (num = GetNumberValue(car(rest))))
      {
        rest = cdr(rest);
      }
      else
      {
        return MakeBool(0);
      }
    }
    return MakeBool(ret);
  }
  if (!strcmp(pName, "<="))
  {
    int num = GetNumberValue(one);
    int ret = 1;
    while (!GetBoolValue(nullp(rest)))
    {
      if (num <= (num = GetNumberValue(car(rest))))
      {
        rest = cdr(rest);
      }
      else
      {
        return MakeBool(0);
      }
    }
    return MakeBool(ret);
  }
  if (!strcmp(pName, "="))
  {
    int num = GetNumberValue(one);
    int ret = 1;
    while (!GetBoolValue(nullp(rest)))
    {
      if (num == (num = GetNumberValue(car(rest))))
      {
        rest = cdr(rest);
      }
      else
      {
        return MakeBool(0);
      }
    }
    return MakeBool(ret);
  }
  if (!strcmp(pName, "-"))
  {
    int ret = GetNumberValue(one);

    if (GetBoolValue(nullp(rest)))
    {
      return MakeNumber(-1 * ret);
    }

    while (!GetBoolValue(nullp(rest)))
    {
      ret -= GetNumberValue(car(rest));
      rest = cdr(rest);
    }

    return MakeNumber(ret);
  }
}

const Sexp apply_one_arg_procedure(char* pName, Sexp args)
{
  if (GetBoolValue(nullp(args)) || !GetBoolValue(nullp(cdr(args))))
  {
    return MakeError("expected 1 argument");
  }

  Sexp one = car(args);

  if (!strcmp(pName, "abs"))
  {
    return MakeNumber(abs(GetNumberValue(one)));
  }
  if (!strcmp(pName, "car"))
  {
    return car(one);
  }
  if (!strcmp(pName, "cadr"))
  {
    return cadr(one);
  }
  if (!strcmp(pName, "cddr"))
  {
    return cddr(one);
  }
  if (!strcmp(pName, "cdr"))
  {
    return cdr(one);
  }
  if (!strcmp(pName, "display"))
  {
    DisplaySexp(one, 0);
    return MakeNull(); // TODO: <void>
  }
  if (!strcmp(pName, "length"))
  {
    return length(one);
  }
  if (!strcmp(pName, "not"))
  {
    if (one.Type == Bool && !GetBoolValue(one))
    {
      return MakeBool(1);
    }
    return MakeBool(0);
  }
  if (!strcmp(pName, "null?"))
  {
    return nullp(one);
  }
  if (!strcmp(pName, "sqrt"))
  {
    return MakeNumber((int)sqrt(GetNumberValue(one)));
  }

  return MakeError("unknown procedure");
}

const Sexp apply_two_arg_procedure(char* pName, Sexp args)
{
  if (GetBoolValue(nullp(args)) || GetBoolValue(nullp(cdr(args))) || !GetBoolValue(nullp(cddr(args))))
  {
    return MakeError("expected 2 argument");
  }

  Sexp one = car(args);
  Sexp two = cadr(args);

  if (!strcmp(pName, "cons"))
  {
    return cons(one, two);
  }

  return MakeError("unknown procedure");
}

Sexp apply_primitive_procedure(Sexp proc, Sexp args)
{
  char* pName = GetSymbolValue(cadr(proc));
  if (!strcmp(pName, "newline"))
  {
    printf("\n");
    return MakeNull(); // TODO: void
  }

  if (!strcmp(pName, "list") ||
      !strcmp(pName, "+") ||
      !strcmp(pName, "*"))
  {
    return apply_zero_or_more_arg_procedure(pName, args);
  }
  if (!strcmp(pName, "-") || 
      !strcmp(pName, ">") ||
      !strcmp(pName, ">=") ||
      !strcmp(pName, "<") ||
      !strcmp(pName, "<=") ||
      !strcmp(pName, "=") || 
      !strcmp(pName, "-"))
  {
    return apply_one_or_more_arg_procedure(pName, args);
  }
  if (!strcmp(pName, "abs") || 
      !strcmp(pName, "car") || 
      !strcmp(pName, "cadr") ||
      !strcmp(pName, "cddr") ||
      !strcmp(pName, "cdr") ||
      !strcmp(pName, "display") ||
      !strcmp(pName, "length") || 
      !strcmp(pName, "not") ||
      !strcmp(pName, "null?") || 
      !strcmp(pName, "sqrt"))
  {
    return apply_one_arg_procedure(pName, args);
  }
  if (!strcmp(pName, "cons"))
  {
    return apply_two_arg_procedure(pName, args);
  }

  // TODO: append, equal?, eq?, integer?, remainder, /

  char buffer[200];
  sprintf_s(buffer, sizeof(buffer), "unknown primitive procedure -- %s", pName);
  return error(buffer);
}

/* 4.1: code from book -- repl */
Sexp setup_environment()
{
  Sexp env = the_empty_environment();
  Sexp p = MakeSymbol("primitive");

  Sexp vars = cons(MakeSymbol("list"), MakeNull());
  vars = cons(MakeSymbol("+"), vars);
  vars = cons(MakeSymbol("*"), vars);
  vars = cons(MakeSymbol(">"), vars);
  vars = cons(MakeSymbol(">="), vars);
  vars = cons(MakeSymbol("<"), vars);
  vars = cons(MakeSymbol("<="), vars);
  vars = cons(MakeSymbol("="), vars);
  vars = cons(MakeSymbol("-"), vars);
  vars = cons(MakeSymbol("abs"), vars);
  vars = cons(MakeSymbol("car"), vars);
  vars = cons(MakeSymbol("cadr"), vars);
  vars = cons(MakeSymbol("cddr"), vars);
  vars = cons(MakeSymbol("cdr"), vars);
  vars = cons(MakeSymbol("display"), vars);
  vars = cons(MakeSymbol("length"), vars);
  vars = cons(MakeSymbol("newline"), vars);
  vars = cons(MakeSymbol("not"), vars);
  vars = cons(MakeSymbol("null?"), vars);
  vars = cons(MakeSymbol("sqrt"), vars);
  vars = cons(MakeSymbol("cons"), vars);

  Sexp vals = cons(list2(p, MakeSymbol("list")), MakeNull());
  vals = cons(list2(p, MakeSymbol("+")), vals);
  vals = cons(list2(p, MakeSymbol("*")), vals);
  vals = cons(list2(p, MakeSymbol(">")), vals);
  vals = cons(list2(p, MakeSymbol(">=")), vals);
  vals = cons(list2(p, MakeSymbol("<")), vals);
  vals = cons(list2(p, MakeSymbol("<=")), vals);
  vals = cons(list2(p, MakeSymbol("=")), vals);
  vals = cons(list2(p, MakeSymbol("-")), vals);
  vals = cons(list2(p, MakeSymbol("abs")), vals);
  vals = cons(list2(p, MakeSymbol("car")), vals);
  vals = cons(list2(p, MakeSymbol("cadr")), vals);
  vals = cons(list2(p, MakeSymbol("cddr")), vals);
  vals = cons(list2(p, MakeSymbol("cdr")), vals);
  vals = cons(list2(p, MakeSymbol("display")), vals);
  vals = cons(list2(p, MakeSymbol("length")), vals);
  vals = cons(list2(p, MakeSymbol("newline")), vals);
  vals = cons(list2(p, MakeSymbol("not")), vals);
  vals = cons(list2(p, MakeSymbol("null?")), vals);
  vals = cons(list2(p, MakeSymbol("sqrt")), vals);
  vals = cons(list2(p, MakeSymbol("cons")), vals);

  env = extend_environment(vars, vals, env);

  // Test for adding built-in procedures defined in scheme. 
  define_variable_bang(MakeSymbol("built-in"),
    list4(MakeSymbol("procedure"),
          MakeNull(),
          list1(MakeNumber(5)),
          env),
    env);

  // hack modified from 2.33 for two list map
  define_variable_bang(MakeSymbol("map2"),
  list4(MakeSymbol("procedure"),
    list3(MakeSymbol("p"), MakeSymbol("s0"), MakeSymbol("s1")),
    Parse("("
          "  (define (accumulate op initial s0 s1)"
          "    (if (or (null? s0) (null? s1))"
          "    initial"
          "    (op (car s0) (car s1)"
          "        (accumulate op initial (cdr s0) (cdr s1)))))"
          "  (accumulate (lambda (x y z) (cons (p x y) z)) '() s0 s1)"
          ")"),
    env),
  env);

  // from 4.1
  define_variable_bang(MakeSymbol("make-begin"),
    list4(MakeSymbol("procedure"),
      list1(MakeSymbol("seq")),
      Parse("("
            "  (cons 'begin seq)"
            ")"),
      env),
    env);

  // from 4.7
  define_variable_bang(MakeSymbol("make-let"),
    list4(MakeSymbol("procedure"),
      list3(MakeSymbol("vars"), MakeSymbol("exps"), MakeSymbol("body")),
      Parse("("
            "  (cons 'let (cons (map2 list vars exps) body))"
            ")"),
      env),
    env);

  // from 4.7
  define_variable_bang(MakeSymbol("let*->nested-lets"),
    list4(MakeSymbol("procedure"),
      list1(MakeSymbol("exp")),
      Parse("("
            "  (define (let*-clauses exp) (cadr exp))"
            "  (define (let*-body exp) (cddr exp))"
            "  (define (let*-clause-var clause) (car clause))"
            "  (define (let*-clause-exp clause) (cadr clause))"
            "  (let ([clauses (let*-clauses exp)]"
            "        [body (let*-body exp)])"
            "    (define (iter clauses)"
            "      (let ([first (car clauses)])"
            "        (make-let (list (let*-clause-var first))"
            "                  (list (let*-clause-exp first))"
            "                  (if (null? (cdr clauses))"
            "                      body"
            "                      (list (iter (cdr clauses)))))))"
            "    (if (null? clauses)"
            "        (make-begin body)"
            "        (iter clauses)))"
            ")"),
      env),
    env);
  return env;
}

/* 4.4: and, or */

Sexp is_andp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("and"));
}

Sexp and_clauses(Sexp exp)
{
  return cdr(exp);
}

Sexp is_orp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("or"));
}

Sexp or_clauses(Sexp exp)
{
  return cdr(exp);
}

/* 4.5: cond arrow */

Sexp make_application(Sexp proc, Sexp arg_list)
{
  return cons(proc, arg_list);
}

Sexp cond_is_arrowp(Sexp clause)
{
  return tagged_listp(cdr(clause), MakeSymbol("=>"));
}

Sexp cond_arrow_proc(Sexp clause)
{
  return caddr(clause);
}

/* 4.6: let */

Sexp is_letp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("let"));
}

Sexp let_to_combination(Sexp exp)
{
  Sexp clauses = cadr(exp);
  Sexp body = cddr(exp);

  // parameters: (map let-clause-var clauses)
  // arglist: (map let-clause-exp clauses)
  Sexp parametersh = MakeNull();
  Sexp arglisth = MakeNull();
  if (!GetBoolValue(nullp(clauses)))
  {
    Sexp clause = car(clauses);

    parametersh = MakePair(car(clause), MakeNull());
    arglisth = MakePair(cadr(clause), MakeNull());
    Sexp parameterst = parametersh;
    Sexp arglistt = arglisth;
    clauses = cdr(clauses);

    while (!GetBoolValue(nullp(clauses)))
    {
      clause = car(clauses);

      Sexp newparameterst = MakePair(car(clause), MakeNull());
      Sexp newarglistt = MakePair(cadr(clause), MakeNull());
      SetCdr(parameterst, newparameterst);
      SetCdr(arglistt, newarglistt);
      parameterst = newparameterst;
      arglistt = newarglistt;

      clauses = cdr(clauses);
    }
  }

  return make_application(make_lambda(parametersh, body), arglisth);
}

/* 4.7: let* */

Sexp is_let_starp(Sexp exp)
{
  return tagged_listp(exp, MakeSymbol("let*"));
}

Sexp let_star_to_nested_lets(Sexp exp)
{
  // FIX!
  return MakeNull();
}

/* 5.32 */

Sexp symbol_applicationp(Sexp exp)
{
  return SexpAnd2(pairp(exp), symbolp(car(exp)));
}

Sexp empty_arglist()
{
  return MakeNull();
}

Sexp adjoin_arg(Sexp arg, Sexp arglist)
{
  return append(arglist, list1(arg));
}

Sexp last_operandp(Sexp ops)
{
  return nullp(cdr(ops));
}

/* 5.32 -- compiling */

Sexp make_compiled_procedure(Sexp entry, Sexp env)
{
  return list3(MakeSymbol("compiled-procedure"), entry, env);
}

Sexp compiled_procedurep(Sexp proc)
{
  return tagged_listp(proc, MakeSymbol("compiled-procedure"));
}

Sexp compiled_procedure_entry(Sexp c_proc)
{
  return cadr(c_proc);
}

Sexp compiled_procedure_env(Sexp c_proc)
{
  return caddr(c_proc);
}

Sexp lexical_address_lookup(Sexp lexical_address, Sexp env)
{
  int skip_frames = GetNumberValue(car(lexical_address));
  int skip_vars = GetNumberValue(cdr(lexical_address));

  for (int i = 0; i < skip_frames; i++)
  {
    if (GetBoolValue(eqp(env, the_empty_environment())))
    {
      error("invalid lexical address");
    }
    env = enclosing_environment(env);
  }

  Sexp vals = frame_values(first_frame(env));
  for (int i = 0; i < skip_vars; i++)
  {
    if (GetBoolValue(nullp(vals)))
    {
      error("invalid lexical address");
    }
    vals = cdr(vals);
  }
  return car(vals);
}

/*

(define (lexical-address-process proc lexical-address env)
  (define (scan-vals skip vals)
    (cond [(null? vals)
           (error "" "invalid lexical address")] ; impossible runtime error (?)
          [(= 0 skip)
           (proc vals)]
          [else
           (scan-vals (- skip 1) (cdr vals))]))
  (define (scan-frames skip env)
    (cond [(eq? env the-empty-environment)
           (error "" "invalid lexical adress")] ; impossible runtime error (?)
          [(= 0 skip)
           (scan-vals (skip-vars lexical-address)
                      (frame-values (first-frame env)))]
          [else
           (scan-frames (- skip 1) (enclosing-environment env))]))
  (scan-frames (skip-frames lexical-address) env))

(define (lexical-address-lookup lexical-address env)
  (lexical-address-process (lambda (vals) (car vals))
                           lexical-address
                           env))

*/

/* Other */
void DisplaySexp(Sexp s, int depth)
{
  if (s.Type == Bool)
  {
    if (GetNumberValue(s) == 0)
    {
      printf("%s", "#f");
    }
    else
    {
      printf("%s", "#t");
    }
  }
  if (s.Type == Error)
  {
    printf("%s", "\"error -- ");
    printf("%s", GetStringValue(s));
    printf("%s", "\"");
  }
  if (s.Type == Null)
  {
    if (depth == 0)
    {
      printf("%s", "()");
    }
    else
    {
      printf("%s", ")");
    }
  }
  if (s.Type == Number)
  {
    printf("%d", GetNumberValue(s));
  }
  if (s.Type == Pair)
  {
    if (depth == 0)
    {
      printf("%s", "(");
    }
    else
    {
      printf("%s", " ");
    }

    if (GetBoolValue(compound_procedurep(s)))
    {
      printf("#<procedure>");
    }
    else if (GetBoolValue(compiled_procedurep(s)))
    {
      printf("#<compiled procedure @ %d>", GetNumberValue(cadr(s)));
    }
    else
    {
      Sexp sCar = GetPairCarValue(s);
      Sexp sCdr = GetPairCdrValue(s);

      DisplaySexp(sCar, 0);
      DisplaySexp(sCdr, depth + 1);
    }

    if (depth == 0)
    {
      printf(")");
    }
  }
  if (s.Type == String)
  {
    // printf("\"");
    printf(GetStringValue(s));
    // printf("\"");
  }
  if (s.Type == Symbol)
  {
    printf(GetStringValue(s));
  }
}

Sexp falsep(Sexp s)
{
  MakeBool(s.Type == Bool && !GetBoolValue(s));
}