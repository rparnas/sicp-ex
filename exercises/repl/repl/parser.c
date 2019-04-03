#include <stdlib.h>;
#include <string.h>;
#include "parser.h";

typedef struct
{
  char* Error;
  Sexp Term;
  int TermIndex;
  struct TermNode* Next;
} TermNode;

static void CleanupTermNode(TermNode* list);
static void AddTermToList(Sexp term, int termIndex, TermNode** list);
static void EmitTerm(char* raw, int termIndex, TermNode** list, int i, int isQuoted);
static int IntegerPower(int base, int exponent);
static TermNode* ParserError(char* message);
static TermNode* ParseInner(char* raw, int start, int stop, int depth);
static int StringIndexOf(char* s, char c, int startIndex);
static Sexp TryParseBool(char* term, int termLength);
static Sexp TryParseNumber(char* term, int termLength);
static Sexp TryParseSymbol(char* term, int termLength);

static void CleanupTermNode(TermNode* list)
{
  while (list != NULL)
  {
    TermNode* head = list;
    list = head->Next;
    free(head);
  }
}

static void AddTermToList(Sexp term, int termIndex, TermNode** list)
{
  TermNode* newNode = malloc(sizeof(TermNode));
  newNode->Error = NULL;
  newNode->Term = term;
  newNode->TermIndex = termIndex;
  newNode->Next = NULL;

  if (*list == NULL)
  {
    *list = newNode;
  }
  else
  {
    TermNode* tail = *list;
    while (tail->Next != NULL)
    {
      tail = tail->Next;
    }

    tail->Next = newNode;
  }
}

static void EmitTerm(char* raw, int termIndex, TermNode** list, int i, int isQuoted)
{
  char* termStr = raw + termIndex;
  int termLength = i - termIndex;
  if (termLength <= 0)
  {
    return;
  }

  Sexp term = MakeNull();
  if (term.Type == Null)
  {
    term = TryParseBool(termStr, termLength); 
  }
  if (term.Type == Null)
  {
    term = TryParseNumber(termStr, termLength); 
  }
  if (term.Type == Null) 
  {
    term = TryParseSymbol(termStr, termLength); 
    if (isQuoted)
    {
      term = MakeQuote(term);
    }
  }

  AddTermToList(term, termIndex, list);
}

static int FindMatchingBracket(char open, char close, char* expression, int startIndex)
{
  int depth = 0;
  int i = startIndex;
  while (expression[i] != '\0')
  {
    if (expression[i] == open)
    {
      depth++;
    }
    else if (expression[i] == close)
    {
      depth--;
    }

    if (depth == 0)
    {
      return i;
    }

    i++;
  }

  return -1;
}

static int IntegerPower(int base, int exponent)
{
  int result = 1;
  for (int i = 0; i < exponent; i++)
  {
    result *= base;
  }
  return result;
}

static TermNode* ParserError(char* message)
{
  TermNode* newNode = malloc(sizeof(TermNode));
  newNode->Error = message;
  newNode->Term = MakeError(message);
  newNode->TermIndex = -1;
  newNode->Next = NULL;
  return newNode;
}

static TermNode* ParseInner(char* raw, int start, int stop, int depth)
{
  // First character index in the raw expression of the term currently being processed.
  int termIndex = start;

  // Tokenized operators and operands indexed by the first character of each term.
  TermNode* ret = NULL;

  // For each character in the raw text
  for (int i = start; i <= stop + 1;)
  {
    int isQuoted = termIndex > start && raw[termIndex - 1] == '\'';

    // If reaching a delimiter at the end of a term.
    if (i == stop + 1 || raw[i] == ' ' || raw[i] == '\r' || raw[i] == '\n' || raw[i] == '\0' || raw[i] == '\'')
    {
      EmitTerm(raw, termIndex, &ret, i, isQuoted);

      i += 1;
      termIndex = i;
    }
    else if (raw[i] == '(' || raw[i] == '[')
    {
      EmitTerm(raw, termIndex, &ret, i, isQuoted);

      int isParensQuoted = i > start && raw[i - 1] == '\'';
      
      char openChar = raw[i];
      char closeChar = raw[i] == '[' ? ']' : ')';

      int matchingBracketIndex = FindMatchingBracket(openChar, closeChar, raw, i);
      if (matchingBracketIndex == -1)
      {
        CleanupTermNode(ret);
        return ParserError("Unmatched parenthesis");
      }

      TermNode* args = ParseInner(raw, i + 1, matchingBracketIndex - 1, depth + 1);
      if (args == NULL)
      {
        if (isParensQuoted)
        {
          AddTermToList(MakeQuote(MakeNull()), i-1, &ret);
        }
        else
        {
          // TODO: should "invalid syntax () a runtime or parsing error?
          AddTermToList(MakeNull(), i, &ret);
        }
      }
      else if (args->Error != NULL)
      {
        CleanupTermNode(ret);
        return args;
      }
      else
      {
        TermNode* argz = args;
        Sexp headCell = MakePair(argz->Term, MakeNull());
        Sexp tailCell = headCell;
        while (argz->Next != NULL)
        {
          argz = argz->Next;

          Sexp cell = MakePair(argz->Term, MakeNull());
          SetCdr(tailCell, cell);
          tailCell = cell;
        }

        Sexp newTerm = headCell;
        int newTermIndex = i;
        if (isParensQuoted)
        {
          newTerm = MakeQuote(headCell);
          newTermIndex--;
        }

        AddTermToList(newTerm, newTermIndex, &ret);
        CleanupTermNode(args);
      }

      i = matchingBracketIndex + 1;
      termIndex = i;
    }
    else if (raw[i] == '"')
    {
      EmitTerm(raw, termIndex, &ret, i, isQuoted);

      int matchingQuoteIndex = StringIndexOf(raw, '"', i + 1);
      if (matchingQuoteIndex == -1)
      {
        CleanupTermNode(ret);
        return ParserError("Unmatched quote");
      }

      Sexp term = MakeStringRaw(raw, i + 1, matchingQuoteIndex - 1);
      AddTermToList(term, i, &ret);

      i = matchingQuoteIndex + 1;
      termIndex = i;
    }
    else
    {
      i++;
    }
  }

  return ret;
}

static int StringIndexOf(char* s, char c, int startIndex)
{
  int i = startIndex;

  while (s[i] != '\0')
  {
    if (s[i] == c)
    {
      return i;
    }

    i++;
  }

  return -1;
}

static Sexp TryParseBool(char* term, int termLength)
{
  if (termLength == 2 && term[0] == '#' && (term[1] == 'f' || term[1] == 't'))
  {
    return MakeBool(term[1] == 't');
  }
  return MakeNull();
}

static Sexp TryParseNumber(char* term, int termLength)
{
  int isNegative = term[0] == '-';
  if (isNegative)
  {
    if (termLength == 1)
    {
      return MakeNull(); // the minus operator is not a number
    }

    term++;
    termLength--;
  }

  int place = IntegerPower(10, termLength - 1);
  int value = 0;

  for (int i = 0; i < termLength; i++)
  {
    char c = term[i];

    if (c == '0') {  }
    else if (c == '1') { value += place * 1; }
    else if (c == '2') { value += place * 2; }
    else if (c == '3') { value += place * 3; }
    else if (c == '4') { value += place * 4; }
    else if (c == '5') { value += place * 5; }
    else if (c == '6') { value += place * 6; }
    else if (c == '7') { value += place * 7; }
    else if (c == '8') { value += place * 8; }
    else if (c == '9') { value += place * 9; }
    else
    {
      return MakeNull();
    }

    place /= 10;
  }

  if (isNegative)
  {
    value *= -1;
  }

  return MakeNumber(value);
}

static Sexp TryParseSymbol(char* term, int termLength)
{
  return MakeSymbolRaw(term, 0, termLength - 1);
}

Sexp Parse(char* raw)
{
  TermNode* list = ParseInner(raw, 0, strlen(raw) - 1, 0);

  Sexp ret;
  if (list == NULL)
  {
    ret = MakeError("Parser returned no expressions");
  }
  else if (list->Next != NULL)
  {
    ret = MakeError("Unexpected second expression");
  }
  else
  {
    ret = list->Term;
  }

  CleanupTermNode(list);
  return ret;
}
