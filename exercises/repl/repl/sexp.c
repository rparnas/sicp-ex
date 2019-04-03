#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sexp.h"

/* Private */

static void *PackInt(int value)
{
  int *ret = malloc(sizeof(int));
  *ret = value;
  return ret;
}

static void *PackCharPtr(char* raw, int startIndex, int stopIndex)
{
  int len = stopIndex - startIndex + 1;
  char* value = malloc((len + 1) * sizeof(char));

  int i = 0;
  for (; i < len; i++)
  {
    value[i] = raw[i + startIndex];
  }
  value[i] = '\0';

  return value;
}

static int UnpackInt(Sexp s) 
{
  return *((int*)s.Value); 
}

static char *UnpackCharPtr(Sexp s)
{
  return (char*)s.Value;
}

/* Cell Management */

const int CellMax = 1000;
static int CellIndex = 0;
static Sexp* Cars = NULL;
static Sexp* Cdrs = NULL;
static Sexp* fCars = NULL;
static Sexp* fCdrs = NULL;
static int fIndex;

const int StackMax = 1000;
int StackIndex = 0;
Sexp Stack[1000];

static void Move(int index, int fIndex)
{
  fCars[fIndex] = Cars[index];
  fCdrs[fIndex] = Cdrs[index];

  Cars[index] = MakeMoved(fIndex);
  Cdrs[index] = MakeMoved(fIndex);
}

static Sexp ReferencePair(int index)
{
  Sexp ret =
  {
    .Type = Pair,
    .Value = PackInt(index)
  };
  return ret;
}

void CGCells()
{
  if (Cars == NULL)
  {
    Cars = malloc(sizeof(Sexp) * CellMax);
    Cdrs = malloc(sizeof(Sexp) * CellMax);
    return;
  }

  if (CellIndex < (CellMax / 2))
  {
    return;
  }

  fCars = malloc(sizeof(Sexp) * CellMax);
  fCdrs = malloc(sizeof(Sexp) * CellMax);
  int fIndex = 0;

  // Process all exprssions on the stack.
  for (int i = 0; i < StackIndex; i++)
  {
    if (Stack[i].Type == Pair)
    {
      int pairIndex = GetNumberValue(Stack[i]);
      if (Cars[pairIndex].Type != Moved)
      {
        Move(pairIndex, fIndex);
        fIndex++;
      }
      Stack[i] = ReferencePair(GetNumberValue(Cars[pairIndex]));
    }
  }

  // Process moved pairs
  for (int i = 0; i < fIndex; i++)
  {
    if (fCars[i].Type == Pair)
    {
      int pairIndex = GetNumberValue(fCars[i]);
      if (Cars[pairIndex].Type != Moved)
      {
        Move(pairIndex, fIndex);
        fIndex++;
      }
      fCars[i] = ReferencePair(GetNumberValue(Cars[pairIndex]));
    }

    if (fCdrs[i].Type == Pair)
    {
      int pairIndex = GetNumberValue(fCdrs[i]);
      if (Cars[pairIndex].Type != Moved)
      {
        Move(pairIndex, fIndex);
        fIndex++;
      }
      fCdrs[i] = ReferencePair(GetNumberValue(Cars[pairIndex]));
    }
  }

  free(Cars);
  free(Cdrs);
  Cars = fCars;
  Cdrs = fCdrs;
  CellIndex = fIndex;
}

void save(Sexp s)
{
  if (StackIndex == StackMax)
  {
    printf("stack overflow!\n");
  }
  Stack[StackIndex] = s;
  StackIndex++;
}

Sexp restore()
{
  StackIndex--;
  return Stack[StackIndex];
}

/* Public */

int AreSamePairInStore(Sexp a, Sexp b)
{
  return GetNumberValue(a) == GetNumberValue(b);
}

int GetBoolValue(Sexp s)
{
  return UnpackInt(s);
}

char* GetErrorValue(Sexp s) 
{
  return UnpackCharPtr(s);
}

int GetNumberValue(Sexp s)
{
  return UnpackInt(s);
}

Sexp GetPairCarValue(Sexp s)
{
  return Cars[UnpackInt(s)];
}

Sexp GetPairCdrValue(Sexp s)
{
  return Cdrs[UnpackInt(s)];
}

char* GetStringValue(Sexp s)
{
  return UnpackCharPtr(s);
}

char* GetSymbolValue(Sexp s)
{
  return UnpackCharPtr(s);
}

Sexp MakeBool(int value)
{
  Sexp ret =
  {
    .Type = Bool,
    .Value = PackInt(value)
  };
  return ret;
}

Sexp MakeError(char* message)
{
  printf("Error: %s\n", message);

  Sexp ret =
  {
    .Type = Error,
    .Value = PackCharPtr(message, 0, strlen(message) - 1)
  };
  return ret;
}

Sexp MakeErrorRaw(char* raw, int startIndex, int stopIndex)
{
  Sexp ret =
  {
    .Type = Error,
    .Value = PackCharPtr(raw, startIndex, stopIndex)
  };
  return ret;
}

Sexp MakeMoved(int newLocation)
{
  Sexp ret =
  {
    .Type = Moved,
    .Value = PackInt(newLocation)
  };
  return ret;
}

Sexp MakeNull()
{
  Sexp ret = { .Type = Null };
  return ret;
}

Sexp MakeNumber(int value)
{
  Sexp ret =
  {
    .Type = Number,
    .Value = PackInt(value)
  };
  return ret;
}

Sexp MakePair(Sexp car, Sexp cdr)
{
  if (CellIndex == CellMax)
  {
    printf("Out of cons cell memory!\n");
  }

  Cars[CellIndex] = car;
  Cdrs[CellIndex] = cdr;

  Sexp ret =
  {
    .Type = Pair,
    .Value = PackInt(CellIndex)
  };

  CellIndex++;

  return ret;
}

Sexp MakeQuote(Sexp s)
{
  return MakePair(MakeSymbol("quote"),
           MakePair(s, MakeNull()));
}

Sexp MakeString(char* string)
{
  Sexp ret =
  {
    .Type = String,
    .Value = PackCharPtr(string, 0, strlen(string) - 1)
  };
  return ret;
}

Sexp MakeStringRaw(char* raw, int startIndex, int stopIndex)
{
  Sexp ret =
  {
    .Type = String,
    .Value = PackCharPtr(raw, startIndex, stopIndex)
  };
  return ret;
}

Sexp MakeSymbol(char* name)
{
  Sexp ret =
  {
    .Type = Symbol,
    .Value = PackCharPtr(name, 0, strlen(name) - 1)
  };
  return ret;
}

Sexp MakeSymbolRaw(char* raw, int startIndex, int stopIndex)
{
  Sexp ret =
  {
    .Type = Symbol,
    .Value = PackCharPtr(raw, startIndex, stopIndex)
  };
  return ret;
}

void SetCar(Sexp s, Sexp newcar)
{
  Cars[UnpackInt(s)] = newcar;
}

void SetCdr(Sexp s, Sexp newcdr)
{
  Cdrs[UnpackInt(s)] = newcdr;
}

Sexp SexpAnd2(Sexp a, Sexp b)
{
  if (a.Type == Bool && !GetBoolValue(a))
  {
    return a;
  }
  return b;
}

Sexp SexpOr2(Sexp a, Sexp b)
{
  if (a.Type == Bool && !GetBoolValue(a))
  {
    return b;
  }
  return a;
}
