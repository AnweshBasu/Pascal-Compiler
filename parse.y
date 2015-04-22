%{     /* pars1.y    Pascal Parser      Gordon S. Novak Jr.  ; 30 Jul 13   */

/* NAME : David Parker
   EID  : dp24559
   Project 3 - Parser */

/* Copyright (c) 2013 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/* 14 Feb 01; 01 Oct 04; 02 Mar 07; 27 Feb 08; 24 Jul 09; 02 Aug 12 */

/*
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.
  */


/* NOTE:   Copy your lexan.l lexical analyzer to this directory.      */

       /* To use:
                     make pars1y              has 1 shift/reduce conflict
                     pars1y                   execute the parser
                     i:=j .
                     ^D                       control-D to end input

                     pars1y                   execute the parser
                     begin i:=j; if i+j then x:=a+b*c else x:=a*b+c; k:=i end.
                     ^D

                     pars1y                   execute the parser
                     if x+y then if y+z then i:=j else k:=2.
                     ^D

           You may copy pars1.y to be parse.y and extend it for your
           assignment.  Then use   make parser   as above.
        */

        /* Yacc reports 1 shift/reduce conflict, due to the ELSE part of
           the IF statement, but Yacc's default resolves it in the right way.*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "token.h"
#include "lexan.h"
#include "symtab.h"
#include "parse.h"

        /* define the type of the Yacc stack element to be TOKEN */

#define YYSTYPE TOKEN

TOKEN parseresult;

%}

/* Order of tokens corresponds to tokendefs.c; do not change */

%token IDENTIFIER STRING NUMBER   /* token types */

%token PLUS MINUS TIMES DIVIDE    /* Operators */
%token ASSIGN EQ NE LT LE GE GT POINT DOT AND OR NOT DIV MOD IN

%token COMMA                      /* Delimiters */
%token SEMICOLON COLON LPAREN RPAREN LBRACKET RBRACKET DOTDOT

%token ARRAY BEGINBEGIN           /* Lex uses BEGIN */
%token CASE CONST DO DOWNTO ELSE END FILEFILE FOR FUNCTION GOTO IF LABEL NIL
%token OF PACKED PROCEDURE PROGRAM RECORD REPEAT SET THEN TO TYPE UNTIL
%token VAR WHILE WITH


%%

  program    :  PROGRAM IDENTIFIER LPAREN IDENTIFIER RPAREN SEMICOLON Lblock DOT { parseresult = 
                                                                   program($1, $2, makeprogn($3, $4), $7);}
             ;
  statement  :  BEGINBEGIN statement endpart
                                       {$$ = makeprogn($1,cons($2, $3)); }
             |  IF expr THEN statement endif   {$$ = makeif($1, $2, $4, $5); }
             |  assignment
             |  function
             |  FOR varid ASSIGN expr TO expr DO statement {$$ = forloop($1,$2,$3,$4,$5,$6,$8);}
             |  REPEAT statements UNTIL expr {$$ = makerepeat($1, $2, $3, $4);}
             |  NUMBER COLON statement { $$ = findlabel($1, $3); }
             |  WHILE expr DO statement { $$ = makewhile($1,$2,$3,$4); }
             |  GOTO NUMBER { $$ = makegoto(getLabelNum($2->intval)); }
             ;
  statements :  statement SEMICOLON statements {$$ = cons($1,$3);}
             |  statement
             ;
 function    :  identifier LPAREN args RPAREN {$$ = function($1, $2, $3);}
             ;
  args       :  expr COMMA args {$$ = cons($1, $3);}
             |  expr {$$ = $1;}
             ;
  varid      :  IDENTIFIER {$$ = varid($1);}
             ;
  Lblock     :  LABEL labelblock Cblock {$$ = $3;}
             |  Cblock
             ;
  labelblock :  integerlist SEMICOLON {makelabel($1);}
             ;
  Cblock     :  CONST constblock Tblock {$$ = $3;}
             |  Tblock
             ;
  Tblock     :  TYPE typeblock Vblock {$$ = $3;}
             |  Vblock
             ;
  typeblock  :  typelist typeblock
             |  typelist
             ;
  typelist   : IDENTIFIER EQ type SEMICOLON {maketype($1, $3);}
             ;
  Vblock     :  VAR varblock block {$$ = $3;}
             |  block
             ;
  block      : BEGINBEGIN statement endpart {$$ = makeprogn($1,cons($2, $3));} 
             ;
  constblock :  constlist constblock
             |  constlist 
             ;
  constlist  :  IDENTIFIER EQ constant SEMICOLON {makeconst($1,$3);}
             ;
  integerlist:  NUMBER COMMA integerlist {$$ = cons($1, $3);}
             |  NUMBER {$$ = $1; }
             ;
  constant   :  NUMBER {$$ = $1; }
             |  STRING {$$ = $1; }
             ;
  varblock   :  varlist varblock
             |  varlist
             ;
  varlist    :  identifiers COLON type SEMICOLON {makevars($1, $3);}
             ;
  identifiers:  IDENTIFIER COMMA identifiers {$$ = cons($1, $3);}
             |  IDENTIFIER {$$ = cons($1, NULL);}
             ;
  type       :  simpletype
             |  RECORD fieldlist END {$$ = makerecord($2);}
             |  POINT IDENTIFIER {$$ = instpoint($1, $2);}
             |  ARRAY LBRACKET simpletypes RBRACKET OF type { $$ = instarray($3, $6); }
             ;
  fieldlist  :  identifiers COLON type SEMICOLON fieldlist {$$ = combinelists(opscons($1,$3), $5);}
             |  identifiers COLON type {$$ = opscons($1,$3);}
             ;
  simpletype :  IDENTIFIER {$$ = findtype($1);}
             |  LPAREN identifiers RPAREN {$$ = instenum($2); }
             |  constant DOTDOT constant {$$ = makesubrange($1->intval, $3->intval); }
             ;
  simpletypes:  simpletype COMMA simpletypes {$$ = cons($1,$3);}
             |  simpletype
             ;
  endpart    :  SEMICOLON statement endpart    { $$ = cons($2, $3); }
             |  END                            { $$ = NULL; }
             ;
  endif      :  ELSE statement                 { $$ = $2; }
             |  /* empty */                    { $$ = NULL; }
             ;
  assignment :  variable ASSIGN expr         { $$ = binop($2, $1, $3); }
             ;
  expressions: expr COMMA expressions {$$ = cons($1, $3); }
             | expr
             ;
  expr       :  expr PLUS term                 { $$ = binop($2, $1, $3); }
             |  expr MINUS term                { $$ = binop($2, $1, $3); }
             |  expr EQ term                   { $$ = binop($2, $1, $3); }
             |  expr NE term                   { $$ = binop($2, $1, $3); }
             |  expr LT term                   { $$ = binop($2, $1, $3); }
             |  expr GT term                   { $$ = binop($2, $1, $3); }
             |  expr GE term                   { $$ = binop($2, $1, $3); }
             |  term 
             ;
  term       :  term TIMES factor              { $$ = binop($2, $1, $3); }
             |  factor {$$ = $1;}
             ;
  factor     :  LPAREN expr RPAREN             { $$ = $2; }
             |  variable
             |  NUMBER
             |  MINUS variable {opscons($1,$2);}
             |  MINUS NUMBER     {opscons($1,$2);}
             |  STRING
             |  function 
             ;
  identifier : IDENTIFIER {$$ = findidentifier($1); }
             | NIL { $$ = constant(0); }
             ;
  variable   : identifier {$$ = $1; }
             | variable DOT IDENTIFIER { $$ = reducedot($1,$2,$3);}
             | variable POINT { $$ = dopoint($1,$2); }
             | variable LBRACKET expressions RBRACKET {$$ = arrayref($1,$2,$3,$4);}
             ;
  // deref      : deref POINT  defre{$$ = $1;}
  //            | POINT
  //            ; 
  // deref      : DPOINTER
  //            | DDOT
  //            | IDENTIFIER {$$ = findidentifier($1);}
  //            ;
  // DPOINTER   : IDENTIFIER POINT deref {$$ = cons(dopoint($1,$2),$3);}
  //            ;
  // DDOT       : deref DOT IDENTIFIER {$$ = cons($1,reducedot($1,$2,$3)); }
             //;










  // deref      : IDENTIFIER POINT DOT deref {$$ = cons(dopoint($1, $2),$4);}
  //            | IDENTIFIER POINT DOT {$$ = dopoint($1, $2);}
  //            | IDENTIFIER DOT IDENTIFIER deref{$$ = cons(reducedot($1, $2, $3),$4); }
  //            | IDENTIFIER DOT IDENTIFIER {$$ = reducedot($1, $2, $3); }
  //            | IDENTIFIER {$$ = findidentifier($1);}
  //            ;

%%

/* You should add your own debugging flags below, and add debugging
   printouts to your programs.

   You will want to change DEBUG to turn off printouts once things
   are working.
  */

#define DEBUG        0             /* set bits here for debugging, 0 = off  */
#define DB_CONS       1             /* bit to trace cons */
#define DB_BINOP      2             /* bit to trace binop */
#define DB_MAKEIF     4             /* bit to trace makeif */
#define DB_MAKEPROGN  8             /* bit to trace makeprogn */
#define DB_PARSERES  16             /* bit to trace parseresult */
#define EXIT 0

 int labelnumber = 0;  /* sequential counter for internal label numbers */

   /*  Note: you should add to the above values and insert debugging
       printouts in your routines similar to those that are shown here.     */

TOKEN cons(TOKEN item, TOKEN list)           /* add item to front of list */
  { item->link = list;
    if (DEBUG & DB_CONS)
       { printf("cons\n");
         dbugprinttok(item);
         dbugprinttok(list);
       };
    return item;
  }

/* Adds an item at the end of the list */
TOKEN combinelists(TOKEN list1, TOKEN list2) {
  TOKEN list1p = list1;

  while(list1p->link != NULL) {
    list1p = list1p->link;
  } 

  list1p->link = list2;

  return list1;
}

TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs)        /* reduce binary operator */
  { 
    if(EXIT) printf("ENTERING binop\n");

    /* OPTIMIZATION : If binop tokens are just constant numbers, don't create a tree */
    if((lhs->tokentype == NUMBERTOK && lhs->datatype == INTEGER) && (rhs->tokentype == NUMBERTOK && rhs->datatype == INTEGER)) {
      if(op->whichval == PLUSOP) return constant(lhs->intval + rhs->intval);
      else if(op->whichval == MINUSOP) return constant(lhs->intval - rhs->intval);
      else if(op->whichval == TIMESOP) return constant(lhs->intval * rhs->intval);
      else if(op->whichval == DIVIDEOP && rhs->intval != 0) return constant(lhs->intval / rhs->intval);
    }

    /* OPTIMIZATION : Coalesce children's add operations */
    else if((lhs->tokentype == NUMBERTOK && lhs->datatype == INTEGER || rhs->tokentype == NUMBERTOK && rhs->datatype == INTEGER) && (op->whichval == PLUSOP /*|| op->whichval == MINUSOP*/)) {
      int ourVal;
      TOKEN other, otherSide,otherlhs,otherrhs,otherop;
      if(lhs->tokentype == NUMBERTOK && lhs->datatype == INTEGER) {
        ourVal = lhs->intval;
        other = rhs;
      } 
      else{
        ourVal = rhs->intval;
        other = lhs;
      } 

      if(other->tokentype == OPERATOR) {
        otherlhs = other->operands;
        otherrhs = other->operands->link;
        otherop = other;

        if((otherlhs->tokentype == NUMBERTOK && otherlhs->datatype == INTEGER || otherrhs->tokentype == NUMBERTOK && otherrhs->datatype == INTEGER) && (otherop->whichval == PLUSOP /*|| other->op->whichval == MINUSOP*/)) {
          if(otherlhs->tokentype == NUMBERTOK && otherlhs->datatype == INTEGER)
            otherSide = otherlhs;
          else
            otherSide = otherrhs;
          otherSide->intval += ourVal;
          return other;
        }
      }
    }

    if(op->tokentype == OPERATOR && op->whichval == ASSIGNOP) {
      /* Convert the rhs to an int */
      if(lhs->datatype == INTEGER && rhs->datatype == REAL) {
        rhs = makeFix(rhs);
        op->datatype = INTEGER;
      }
      /* Convert the rhs to a float */
      else if (lhs->datatype == REAL && rhs->datatype == INTEGER) {
        rhs = makefloat(rhs);
        op->datatype = REAL;
      }
    }

    else {
      /* Cast (fix) rhs to int */
      if(lhs->datatype == INTEGER && rhs->datatype == REAL) {
        lhs = makefloat(lhs);
        op->datatype = REAL;
      }
      /* Cast rhs to float */
      else if (lhs->datatype == REAL && rhs->datatype == INTEGER) {
        rhs = makefloat(rhs);
        op->datatype = REAL;
      }
    }

  op->operands = lhs;          /* link operands to operator       */
    lhs->link = rhs;             /* link second operand to first    */
    rhs->link = NULL;            /* terminate operand list          */
    if (DEBUG & DB_BINOP)
       { printf("binop\n");
         dbugprinttok(op);
         dbugprinttok(lhs);
         dbugprinttok(rhs);
       };
       if(EXIT) printf("LEAVING binop\n");
    return op;
  }

TOKEN makeif(TOKEN tok, TOKEN exp, TOKEN thenpart, TOKEN elsepart)
  {  tok->tokentype = OPERATOR;  /* Make it look like an operator   */
     tok->whichval = IFOP;
     if (elsepart != NULL) elsepart->link = NULL;
     thenpart->link = elsepart;
     exp->link = thenpart;
     tok->operands = exp;
     if (DEBUG & DB_MAKEIF)
        { printf("makeif\n");
          dbugprinttok(tok);
          dbugprinttok(exp);
          dbugprinttok(thenpart);
          dbugprinttok(elsepart);
        };
     return tok;
   }

TOKEN findidentifier(TOKEN tok) {
  if(EXIT) printf("ENTERING findidentifier\n");

  SYMBOL sym = searchst(tok->stringval);
  if(sym != NULL) {
    if(sym->kind == CONSTSYM) {
      tok->tokentype = NUMBERTOK;
      tok->datatype = sym->basicdt;
      tok->symentry = sym;

      if(sym->basicdt == INTEGER) {
        tok->intval = sym->constval.intnum;
      }
      else if(sym->basicdt == REAL) {
        tok->realval = sym->constval.realnum;
      }
      else if(sym->basicdt == STRING) {
        strcpy(sym->constval.stringconst, tok->stringval);
      }
      if(EXIT) printf("LEAVING findidentifier\n");
      return tok;
    }

    else if (sym->kind == VARSYM) {
      tok->tokentype = IDENTIFIERTOK;
      tok->datatype = sym->basicdt;
      tok->symentry = skipTypes(sym->datatype);
      tok->symtype = tok->symentry;

       if(EXIT) printf("LEAVING findidentifier\n");
      return tok;
    }
    if(EXIT) printf("LEAVING findidentifier\n");
    else return tok;
  }
  if(EXIT) printf("LEAVING findidentifier\n");
  else return tok;
}

SYMBOL skipTypes(SYMBOL sym) {
  while(sym->kind == TYPESYM) {
    sym = sym->datatype;
  }
  return sym;
}

TOKEN makeprogn(TOKEN tok, TOKEN statements)
  {  tok->tokentype = OPERATOR;
     tok->whichval = PROGNOP;
     tok->operands = statements;
     if (DEBUG & DB_MAKEPROGN)
       { printf("makeprogn\n");
         dbugprinttok(tok);
         dbugprinttok(statements);
       };
     return tok;
   }


/* My functions */
TOKEN makevars(TOKEN list, TOKEN type) {
  SYMBOL symtype = type->symtype;
  instvars(list, type);

  if(EXIT) printf("LEAVING MAKEVARS\n");
  return type;
}

TOKEN makeconst(TOKEN id, TOKEN value) {
  SYMBOL sym = insertsym(id->stringval);
  sym->kind = CONSTSYM;
  sym->basicdt = value->datatype;
  sym->size = sizeof(sym->basicdt);

  if(sym->basicdt == INTEGER) {
    sym->constval.intnum = value->intval;
    id->tokentype = NUMBERTOK;
  }
  else if(sym->basicdt == REAL) {
    sym->constval.realnum = value->realval;
    id->tokentype = NUMBERTOK;
  }
  else if(sym->basicdt == STRING) {
    strcpy(sym->constval.stringconst, value->stringval);
    id->tokentype = STRINGTOK;
  }

  return id;
}

TOKEN makelabel(TOKEN intlist) {
  while(intlist != NULL) {
    labels[labelnumber++] = intlist->intval;
    TOKEN temp = intlist->link;
    intlist->link = NULL;
    intlist = temp;
  }

  return intlist;
}

/* Get a label's internal number from the labels array */
int getLabelNum(int num) {
  int ret = 0;
  int i;
  for(i = 0; i < 50; i++) {
    if(labels[i] == num) {
      ret = i;
    }
  }

  return ret;
}

TOKEN maketype(TOKEN id, TOKEN type) {
  SYMBOL typesym;
  /* This type token is an unamed type */
  if(!strcmp(type->stringval,"")) {
    typesym = type->symtype;
  }
  else {
    typesym = searchst(type->stringval);
  }
  /* Has this type already been declared? If so, define it */
  SYMBOL sym = searchst(id->stringval);

  if(sym == NULL) {
   sym = inserttype(id->stringval, typesym->size);
   sym->datatype = typesym;
  }

  else {
    // printf("Already Declared, %s Type sym size: %d\n", id->stringval, typesym->size);
      sym->kind = TYPESYM;
      sym->size = typesym->size;
      sym->datatype = typesym;
  }
  sym->basicdt = typesym->basicdt;
  return id;
}

TOKEN makerecord(TOKEN fieldlist) {
  SYMBOL top = NULL;
  SYMBOL last = NULL;
  int totalSize = 0;
  int offset = 0;

  while(fieldlist != NULL) {
    SYMBOL typesym = searchst(fieldlist->operands->stringval);

    SYMBOL entry = makesym(fieldlist->stringval);
    if(last != NULL) last->link = entry;
    last = entry;
    if(top == NULL) top = entry;

    /* Align entries to a multiple of their size */
    offset += padding(offset, typesym->size);
    entry->offset = offset;
    offset += typesym->size;
    entry->size = typesym->size;
    entry->datatype = typesym;
    entry->basicdt = typesym->basicdt;

    /* Move to the next token that does not contain the type operand */
    fieldlist = fieldlist->link;

    /* Loop on each same type in the field list */
    while(fieldlist != NULL && fieldlist->operands == NULL) {
      SYMBOL entry = makesym(fieldlist->stringval);
      if(last != NULL) last->link = entry;
      last = entry;

      /* Align entries to a multiple of their size */
      offset += padding(offset, typesym->size);
      entry->offset = offset;
      offset += typesym->size;
      entry->size = typesym->size;
      entry->datatype = typesym;
      entry->basicdt = typesym->basicdt;

      fieldlist = fieldlist->link;
    }
  }

  /* Add the final padding to align the record to 8 bytes */
    SYMBOL recordsym = symalloc();
    recordsym->kind = RECORDSYM;
    recordsym->link = NULL;
    totalSize = offset + padding(offset, alignsize(recordsym));

    /* Set the recordsym's datatype to be the top of the record */
    recordsym->datatype = top;
    recordsym->size = totalSize;

    TOKEN ret = talloc();
    ret->symtype = recordsym;

  return ret;
}

TOKEN makesubrange(int low, int high) {
  SYMBOL subrange = symalloc();
  subrange->kind = SUBRANGE;
  subrange->basicdt = INTEGER;
  subrange->lowbound = low;
  subrange->highbound = high;
  subrange->size = basicsizes[INTEGER];
  /* Add for referencing self in array */
  subrange->datatype = subrange;

  TOKEN tok = talloc();
  tok->symtype = subrange;
  tok->tokentype = NUMBERTOK;

  return tok;
}

TOKEN makewhile(TOKEN tok, TOKEN expr, TOKEN tokb, TOKEN statement) {
  TOKEN labeltok = label();
  TOKEN ret = makeprogn(tok, labeltok);
  ret->operands->link = makeif(talloc(), expr, statement, NULL);

  /* Statement is a progn */
  statement = statement->operands;

  while(statement->link != NULL) {
    statement = statement->link;
  }

  statement->link = makegoto(labeltok->datatype);

  return ret;
}

TOKEN instenum(TOKEN idlist) {
  int size = 0;

  while(idlist != NULL) {
    TOKEN value = constant(size);
    makeconst(idlist,value);

    idlist = idlist->link;
    size++;
    free(value);
  }

  return makesubrange(0, size - 1);
}

/* Creates a pointer sym for this token */
TOKEN instpoint(TOKEN tok, TOKEN typename) {
  SYMBOL pointer = symalloc();
  pointer->kind = POINTERSYM;
  pointer->datatype = searchins(typename->stringval);
  pointer->basicdt = POINTER;
  pointer->size = basicsizes[POINTER];

  tok = talloc();
  tok->symtype = pointer;

  return tok;
}

TOKEN instarray(TOKEN simpletypes, TOKEN typetok) {
  SYMBOL sym = multidem(simpletypes, typetok);

  TOKEN ret = talloc();
  ret->symtype = sym;
  return ret;
}

SYMBOL multidem(TOKEN simpletypes, TOKEN typetok) {
  if(simpletypes == NULL) return typetok->symtype;

  SYMBOL sym = symalloc();
  SYMBOL simplesym = simpletypes->symtype;
  sym->kind = ARRAYSYM;

  /* Subrange */
  if(simplesym->datatype != NULL) {
    sym->lowbound = simplesym->datatype->lowbound;
    sym->highbound = simplesym->datatype->highbound;
  }

  sym->datatype = multidem(simpletypes->link, typetok);
  sym->size = (sym->highbound - sym->lowbound + 1)*sym->datatype->size;

  return sym;
}

/* Dereference a pointer and return the ^ as the top */
TOKEN dopoint(TOKEN var, TOKEN tok) {
  SYMBOL sym = var->symtype;

  tok = createtok(OPERATOR,POINTEROP);
  tok->operands = var;

  sym = skipTypes(sym);

  tok->symtype = sym;

  return tok;
}

/* Returns an aref for a record dereference */
TOKEN reducedot(TOKEN var, TOKEN dot, TOKEN field) {
  SYMBOL record = var->symtype;
  bool ispointer = false;

  /* Skip to the pointer's RECORDSYM symbol */
  if(record->kind == POINTERSYM) {
    record = record->datatype;
    record = skipTypes(record);
    ispointer = true;
  }
  int oldOffset = 0;
  bool reuse = false;

  /* OPTIMIZATION : Combine aref's whose offsets are constant numbers */
  if(!ispointer && var->tokentype == OPERATOR && var->whichval == AREFOP && var->operands->link->tokentype == NUMBERTOK) {
    dot = var;
    oldOffset = var->operands->link->intval;
    reuse = true;
  }

  else {
    dot = createtok(OPERATOR,AREFOP);
  }

  /* Move to the first record entry */
  record = record->datatype;
  // printf("Reduce Dot %s\n", record->namestring);

  while(record != NULL && strcmp(field->stringval, record->namestring)) {
    record = record->link;
  }

  dot->datatype = record->basicdt;
  int offset = record->offset + oldOffset;

  if(!ispointer && offset == 0) {
    return var;
  }

  if(!reuse) {
    dot->operands = var;
  }

  dot->operands->link = constant(offset);

  dot->symtype = skipTypes(record->datatype);
  return dot;
}

/* Returns an aref for an array dereference */
TOKEN arrayref(TOKEN arr, TOKEN tok, TOKEN subs, TOKEN tokb) {
  int index = 0;
  int offset = 0;
  SYMBOL array = arr->symtype;
  TOKEN offsetTok = NULL;
  TOKEN last = NULL;

  /* Go through all the dimensions of the array */
  while(subs != NULL) {
    TOKEN link = subs->link;
    int size = array->datatype->size;

    /* Creates a binop for every operation, which has been optimized to reduce constants
       to a single number instead of returning the op as the root */
    TOKEN mul = binop(createtok(OPERATOR,TIMESOP), constant(size), subs);
    TOKEN add = binop(createtok(OPERATOR,PLUSOP), constant(-size * array->lowbound), mul);

    if(offsetTok != NULL) {
      TOKEN addlast = binop(createtok(OPERATOR,PLUSOP), offsetTok, add);
      offsetTok = addlast;
    }

    else {
      offsetTok = add;
    }
    /* Move to the next array */
    array = array->datatype;
    subs = link;
  }

  TOKEN ret = createtok(OPERATOR,AREFOP);
  array = skipTypes(array);
  ret->operands = arr;
  ret->operands->link = offsetTok;
  ret->symtype = array;

  return ret;
}

TOKEN makefloat(TOKEN tok) {
  SYMBOL sym = searchst(tok->stringval);
  TOKEN cast = talloc();

  /* Couldn't find a symobl by it's name, probably a constant */
  if(sym == NULL && tok->symentry != NULL) {
    sym = searchst(tok->symentry->namestring);
  }

  if(sym == NULL || sym->kind == CONSTSYM) {
    cast->tokentype = NUMBERTOK;
    cast->datatype = REAL;
    cast->realval = (float)tok->intval;
    return cast;
  } 

  else if(sym->kind == VARSYM) {
    cast->tokentype = OPERATOR;
    cast->whichval = FLOATOP;
    cast->operands = tok;
  }

  return cast;
}

TOKEN makeFix(TOKEN tok) {
  SYMBOL sym = searchst(tok->stringval);

  TOKEN fix = talloc();
  fix->tokentype = OPERATOR;
  fix->whichval = FIXOP;
  fix->operands = tok;

  return fix;
}

TOKEN findlabel(TOKEN number, TOKEN statement) {
  TOKEN ret, labeltok;
  ret = talloc();
  ret->tokentype = OPERATOR;
  ret->whichval = LABELOP;

  /* Since label doesn't use datatype for anything, store the label number in here */
  int num = 0;
  int i;

  for(i = 0; i < 50; i++) {
    if(labels[i] == number->intval) {
      num = i;
    }
  }
   ret->datatype = number->intval;
   labeltok = constant(num);
   ret->operands = labeltok;
   ret->link = statement;
   ret = makeprogn(number, ret);

  return ret;
}

TOKEN findtype(TOKEN tok) {
  if(tok->tokentype != IDENTIFIERTOK) printf("Identifier expected, type()\n");
  SYMBOL sym = searchst(tok->stringval);
  if(sym == NULL) printf("Type not found in symbol table, type()\n");
  tok->symtype = sym;

  if(EXIT) printf("LEAVING FIND TYPE\n");
  return tok;
}

TOKEN program(TOKEN program, TOKEN identifiers, TOKEN progn, TOKEN block) {
  /* Smash this token from Reserved word into an Operator */
  program->tokentype = OPERATOR;
  program->whichval = PROGRAMOP;
  /* add the tree links */
  program->operands = identifiers;
  identifiers->link = progn;
  progn->link = block;

  if(EXIT) printf("LEAVING PROGRAM\n");
  return program;
}

TOKEN getid(TOKEN tok) {
  SYMBOL sym;
  SYMBOL symtype;
  sym = searchst(tok->stringval);

  if(sym == NULL) {
    printf("Variable doesn't exist, getid()\n");
  }
  else {
    tok->symentry = sym;
    symtype = sym->datatype;
    tok->datatype = symtype->basicdt;
  }

  return tok;
}

TOKEN opscons(TOKEN minus, TOKEN value) {
  /* Add a unary minus to a value */
  minus->operands = value;
  return minus;
}

/* Adds an operands to the end of the list instead of the top */
TOKEN opsconsend(TOKEN list, TOKEN value) {
  /* Add a unary minus to a value */
  TOKEN curr = list;
  while(curr->link != NULL) {
    curr = curr->link;
  }
  curr->operands = value;
  return list;
}

TOKEN varid(TOKEN id) {
  /* Check if this variable is in the symbol table */
  return getid(id);
}

TOKEN forloop(TOKEN fortok, TOKEN varid, TOKEN assign, TOKEN assign_expression, TOKEN smash, TOKEN to_expression, TOKEN statement) {
  if(EXIT) printf("ENTERING forloop\n");
  TOKEN ret = fortok;
  TOKEN labeltok;
  ret = makeprogn(ret, binop(assign, varid, assign_expression));

  TOKEN next = ret->operands;
  TOKEN iftok, gototok, ifexpr, varidcopy, inctok, plustok;

  /* Add the goto label */
  labeltok = label();
  next->link = labeltok;
  next = next->link;


  /* Create the expression for the if */
  varidcopy = copytok(varid);
  ifexpr = createtok(OPERATOR, LEOP);
  ifexpr->operands = varidcopy;
  (ifexpr->operands)->link = to_expression;

  iftok = createtok(OPERATOR, IFOP);
  statement = makeprogn(smash, statement);
  next->link = makeif(iftok, ifexpr, statement, NULL);

  /* Next is on the statement under the progn */
  next = statement->operands;

  /* increment */

  /* Create the plus operator sub tree */
  plustok = createtok(OPERATOR, PLUSOP);
  plustok->operands = copytok(varid);
  (plustok->operands)->link = constant(1);

  inctok = createtok(OPERATOR, ASSIGNOP);
  inctok->operands = copytok(varid);
  (inctok->operands)->link = plustok;

  next->link = inctok;

  /* Add the goto statement */
  next = next->link;
  next->link = makegoto(labeltok->datatype);

  if(EXIT) printf("LEAVING forloop\n");

  return ret;
}

TOKEN makerepeat(TOKEN repeat, TOKEN statements, TOKEN until, TOKEN expr) {
  statements = makeprogn(talloc(),statements);
  TOKEN ret;
  TOKEN next, iftok, labeltok;
  iftok = talloc();

  labeltok = label();
  ret = makeprogn(repeat, labeltok);
  next = ret->operands;
  next->link = statements;

  /* Next is now end of statements */
  while(next->link != NULL) {
    next = next->link;
  }

  /* Add the if statement as the link to the end of the statements */
  next->link = makeif(iftok, expr, until = makeprogn(talloc(),NULL), NULL);

  /* until is a throw away token that we converted into an empty progn */
  until->link = makegoto(labeltok->datatype);
  return ret;
}

TOKEN function(TOKEN id, TOKEN smash, TOKEN args) {
  TOKEN ret = createtok(OPERATOR,FUNCALLOP);
  id = getid(id);

  /* Hard code new and write here */
  if(!strcmp(id->stringval, "new")) {
    TOKEN assign = createtok(OPERATOR,ASSIGNOP);

    /* Find the argument symbol */
    SYMBOL sym = searchst(args->stringval);

    /* Argname -> Pointer Type -> Namlesspointer -> Object pointed to -> size */
    TOKEN size = constant(sym->datatype->datatype->datatype->size);

    id->link = size;
    ret->operands = id;
    ret->datatype = id->datatype;

    args->link = ret;
    assign->operands = args;
    return assign;
  }

  else if(!strcmp(id->stringval, "write")) {
    if(args->datatype == STRING) {
      /* Already Correct */
    }
    else if(args->datatype == INTEGER) {
      strcpy(id->stringval, "writei");
    }

    else if(args->datatype == REAL) {
      strcpy(id->stringval, "writef");
    }

    id->link = args;
    ret->operands = id;
    ret->datatype = id->datatype;

  }
  else if(!strcmp(id->stringval, "writeln")) {
    if(args->datatype == STRING) {
      /* Already Correct */
    }
    else if(args->datatype == INTEGER) {
      strcpy(id->stringval, "writelni");
    }

    else if(args->datatype == REAL) {
      strcpy(id->stringval, "writelnf");
    }

    id->link = args;
    ret->operands = id;
    ret->datatype = id->datatype;

  }

  else {
    id->link = args;
    ret->operands = id;
    ret->datatype = id->datatype;
  }

  return ret;
}

TOKEN label() {
  TOKEN ret, labeltok;
  ret = talloc();
  ret->tokentype = OPERATOR;
  ret->whichval = LABELOP;

  /* Since label doesn't use datatype for anything, store the label number in here */
  ret->datatype = labelnumber;
  labeltok = constant(labelnumber++);
  ret->operands = labeltok;

  return ret;
}

TOKEN makegoto(int label) {
  TOKEN gototok;
  TOKEN ret = talloc();
  ret->tokentype = OPERATOR;
  ret->whichval = GOTOOP;
  gototok = constant(label);
  ret->operands = gototok;

  return ret;
}

TOKEN constant(int number) {
  TOKEN tok = talloc();
  tok->tokentype = NUMBERTOK;
  tok->datatype = INTEGER;
  tok->intval = number;

  return tok;
}

TOKEN createtok(int what, int which) {
  TOKEN ret = talloc();
  ret->tokentype = what;
  ret->whichval = which;

  return ret;
}

TOKEN copytok(TOKEN tok) {
  TOKEN ret = talloc();
  *ret = *tok;
  ret->operands = NULL;
  ret->link = NULL;

  return ret;
}

int wordaddress(int n, int wordsize)
  { return ((n + wordsize - 1) / wordsize) * wordsize; }

void instvars(TOKEN idlist, TOKEN typetok)
{  SYMBOL sym, typesym; int align;
   if (DEBUG)
      { printf("instvars\n");
  dbugprinttok(idlist);
  dbugprinttok(typetok);
};
   typesym = typetok->symtype;
   align = alignsize(typesym);
   while ( idlist != NULL )   /* for each id */
     {  sym = insertsym(idlist->stringval);
        sym->kind = VARSYM;
        sym->offset = wordaddress(blockoffs[blocknumber], align);
        sym->size = typesym->size;
        blockoffs[blocknumber] = sym->offset + sym->size;
        sym->datatype = typesym;
        sym->basicdt = typesym->basicdt; /* some student programs use this */
  idlist = idlist->link;
};
    if (DEBUG) printst();
}

 
yyerror(s)
  char * s;
  { 
  fputs(s,stderr); putc('\n',stderr);
  }

main()
  { int res;
    initsyms();
    res = yyparse();

   printst();
    printf("yyparse result = %8d\n", res);
    if (DEBUG & DB_PARSERES) dbugprinttok(parseresult);
    ppexpr(parseresult);           /* Pretty-print the result tree */
  }
