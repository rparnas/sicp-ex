#pragma once

typedef enum
{
  Bool,
  Error,
  Null,
  Number,
  Pair,
  String,
  Symbol,
  // GC Types
  Moved
} SexpTypes;

typedef struct
{
  SexpTypes Type;
  void* Value;
} Sexp;

void CGCells();
void save(Sexp s);
Sexp restore();

int AreSamePairInStore(Sexp a, Sexp b);
int GetBoolValue(Sexp s);
char* GetErrorValue(Sexp s);
int GetNumberValue(Sexp s);
Sexp GetPairCarValue(Sexp s);
Sexp GetPairCdrValue(Sexp s);
char* GetStringValue(Sexp s);
char* GetSymbolValue(Sexp s);
Sexp MakeBool(int value);
Sexp MakeError(char* message);
Sexp MakeErrorRaw(char* raw, int startIndex, int stopIndex);
Sexp MakeMoved(int newLocation);
Sexp MakeNull();
Sexp MakeNumber(int value);
Sexp MakePair(Sexp car, Sexp cdr);
Sexp MakeQuote(Sexp s);
Sexp MakeString(char* string);
Sexp MakeStringRaw(char* raw, int startIndex, int stopIndex);
Sexp MakeSymbol(char* name);
Sexp MakeSymbolRaw(char* raw, int startIndex, int stopIndex);
void SetCar(Sexp s, Sexp newcar);
void SetCdr(Sexp s, Sexp newcdr);
Sexp SexpAnd2(Sexp a, Sexp b);
Sexp SexpOr2(Sexp a, Sexp b);
