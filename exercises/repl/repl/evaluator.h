#pragma once

#include "sexp.h"

#include "evaluator.h"
#include "scheme.h"
#include <stdlib.h>

/* 4.1: Code from book -- self-evaluating */

Sexp self_evaluatingp(Sexp exp);

/* 4.1: Code from book -- variable */

Sexp variablep(Sexp exp);

/* 4.1: Code from book -- quoted */

Sexp quotedp(Sexp exp);
Sexp text_of_quotation(Sexp exp);

/* 4.1: Code from book -- assignment */

Sexp assignmentp(Sexp exp);
Sexp assignment_variable(Sexp exp);
Sexp assignment_value(Sexp exp);

/* 4.1: Code from book -- definition */

Sexp definitionp(Sexp exp);
Sexp definition_variable(Sexp exp);
Sexp definition_value(Sexp exp);

/* 4.1: Code from book -- if */

Sexp make_if(Sexp predicate, Sexp consequent, Sexp alternative);
Sexp ifp(Sexp exp);
Sexp if_predicate(Sexp exp);
Sexp if_consequent(Sexp exp);
Sexp if_alternative(Sexp exp);

/* 4.1: Code from book -- lambda */
Sexp make_lambda(Sexp parameters, Sexp body);
Sexp lambdap(Sexp exp);
Sexp lambda_parameters(Sexp exp);
Sexp lambda_body(Sexp exp);
Sexp make_procedure(Sexp parameters, Sexp body, Sexp env);
Sexp compound_procedurep(Sexp p);
Sexp procedure_parameters(Sexp p);
Sexp procedure_body(Sexp p);
Sexp procedure_environment(Sexp p);

/* 4.1: Code from book -- begin */

Sexp beginp(Sexp exp);
Sexp begin_actions(Sexp seq);
Sexp last_expp(Sexp seq);
Sexp first_exp(Sexp seq);
Sexp rest_exps(Sexp seq);
Sexp sequence_to_exp(Sexp seq);
Sexp make_begin(Sexp seq);

/* 4.1: Code from book -- cond */

Sexp condp(Sexp exp);
Sexp cond_clauses(Sexp exp);
Sexp cond_else_clausep(Sexp exp);
Sexp cond_predicate(Sexp exp);
Sexp cond_actions(Sexp exp);
Sexp cond_to_if(Sexp exp);
Sexp expand_clauses(Sexp clauses);

/* 4.1: Code from book -- application */

Sexp applicationp(Sexp exp);
Sexp operator(Sexp exp);
Sexp operands(Sexp exp);
Sexp no_operandsp(Sexp ops);
Sexp first_operand(Sexp ops);
Sexp rest_operands(Sexp ops);

/* 4.1: Code from book -- utilities */

Sexp tagged_listp(Sexp exp, Sexp tag);

/* 4.1: Code from book -- environments */

Sexp enclosing_environment(Sexp env);
Sexp first_frame(Sexp env);
Sexp the_empty_environment();
Sexp make_frame(Sexp variables, Sexp values);
Sexp frame_variables(Sexp frame);
Sexp frame_values(Sexp frame);
void add_binding_to_frame_bang(Sexp var, Sexp val, Sexp frame);
Sexp extend_environment(Sexp vars, Sexp vals, Sexp base_env);
static Sexp luscan(Sexp var, Sexp env, Sexp vars, Sexp vals);
static Sexp luenv_loop(Sexp var, Sexp env);
Sexp lookup_variable_value(Sexp var, Sexp env);
static void sscan(Sexp var, Sexp val, Sexp env, Sexp vars, Sexp vals);
static void senv_loop(Sexp var, Sexp val, Sexp env);
void set_variable_value_bang(Sexp var, Sexp val, Sexp env);
static void dscan(Sexp var, Sexp val, Sexp frame, Sexp vars, Sexp vals);
void define_variable_bang(Sexp var, Sexp val, Sexp env);

/* 4.1: primitive procedures */
Sexp primitive_procedurep(Sexp proc);
Sexp primitive_implementation(Sexp proc);
Sexp apply_primitive_procedure(Sexp proc, Sexp args);

/* 4.1: code from book -- repl */
Sexp setup_environment();

/* 4.4: and, or */

Sexp is_andp(Sexp exp);
Sexp and_clauses(Sexp exp);
Sexp is_orp(Sexp exp);
Sexp or_clauses(Sexp exp);

/* 4.5: cond arrow */
Sexp cond_is_arrowp(Sexp clause);
Sexp cond_arrow_proc(Sexp clause);

/* 4.6: let */

Sexp is_letp(Sexp exp);
Sexp let_to_combination(Sexp exp);

/* 4.7: let* */

Sexp is_let_starp(Sexp exp);
Sexp let_star_to_nested_lets(Sexp exp);

/* 5.32 */
Sexp symbol_applicationp(Sexp exp);
Sexp empty_arglist();
Sexp adjoin_arg(Sexp arg, Sexp arglist);
Sexp last_operandp(Sexp ops);

/* 5.32 -- compiling */
Sexp make_compiled_procedure(Sexp entry, Sexp env);
Sexp compiled_procedurep(Sexp proc);
Sexp compiled_procedure_entry(Sexp c_proc);
Sexp compiled_procedure_env(Sexp c_proc);
Sexp lexical_address_lookup(Sexp lexical_address, Sexp env);

/* Other */
void DisplaySexp(Sexp s, int depth);
Sexp falsep(Sexp s);