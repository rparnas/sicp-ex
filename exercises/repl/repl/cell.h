#pragma once

#include "sexp.h"

typedef struct
{
  Sexp* Car;
  Sexp* Cdr;
} Cell;
