%{     /* pars1.y    Pascal Parser      Gordon S. Novak Jr.  ; 30 Jul 13   */

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

  program    :  PROGRAM IDENTIFIER LPAREN IDENTIFIER RPAREN SEMICOLON block DOT { parseresult = 
                                                                   program($1, $2, makeprogn($3, $4), $7);}
             ;
  statement  :  BEGINBEGIN statement endpart
                                       { $$ = makeprogn($1,cons($2, $3)); }
             |  IF expr THEN statement endif   { $$ = makeif($1, $2, $4, $5); }
             |  assignment
             |  function
             |  FOR varid ASSIGN expr TO expr DO statement {$$ = forloop($1,$2,$3,$4,$5,$6,$8);}
             ;
  function   :  IDENTIFIER LPAREN args RPAREN {$$ = function($1, $2, $3);}
             ;
  args       :  expr COMMA expr {$$ = cons($1, $3);}
             |  expr {$$ = $1;}
             ;
  varid      :  IDENTIFIER {$$ = varid($1);}
             ;
  block      :  var block {$$ = $2;}
             |  BEGINBEGIN statement endpart {$$ = makeprogn($1,cons($2, $3));}
             ;
  var        :  VAR identifiers COLON type SEMICOLON {makevars($2, $4);}
             ;
  identifiers:  IDENTIFIER COMMA identifiers {$$ = cons($1, $3);}
             |  IDENTIFIER {$$ = cons($1, NULL);}
             ;
  type       :  simpletype
             ;
  simpletype :  IDENTIFIER {$$ = findtype($1);}
             ;
  endpart    :  SEMICOLON statement endpart    { $$ = cons($2, $3); }
             |  END                            { $$ = NULL; }
             ;
  endif      :  ELSE statement                 { $$ = $2; }
             |  /* empty */                    { $$ = NULL; }
             ;
  assignment :  IDENTIFIER ASSIGN expr         { $$ = binop($2, $1, $3); }
             ;
  expr       :  expr PLUS term                 { $$ = binop($2, $1, $3); }
             |  term 
             ;
  term       :  term TIMES factor              { $$ = binop($2, $1, $3); }
             |  factor
             ;
  factor     :  LPAREN expr RPAREN             { $$ = $2; }
             |  IDENTIFIER
             |  NUMBER
             |  STRING
             ;

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

TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs)        /* reduce binary operator */
  { op->operands = lhs;          /* link operands to operator       */
    lhs->link = rhs;             /* link second operand to first    */
    rhs->link = NULL;            /* terminate operand list          */
    if (DEBUG & DB_BINOP)
       { printf("binop\n");
         dbugprinttok(op);
         dbugprinttok(lhs);
         dbugprinttok(rhs);
       };
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
  // printf("%d\n\n\n", symtype->basicdt);
  while(list != NULL) {
        if(list->tokentype != IDENTIFIERTOK) printf("Identifier expected, var_decl()\n");
        if(searchlev(list->stringval,blocknumber) == NULL) {
            SYMBOL newVar = insertsym(list->stringval);
            newVar->kind = VARSYM;
            newVar->size = symtype->size;
            newVar->datatype = symtype;
            newVar->basicdt = symtype->basicdt;
            // list->symtype = type->symentry;
            list = list->link;
        }
        else {
            printf("Attempt to redeclare variable %s\n",list->stringval);
        }
    }

    if(EXIT) printf("LEAVING MAKEVARS\n");
    return type;
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
  // printf("Program is type %d and val %d\n", program->tokentype, program->whichval);

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
    if(symtype->kind == BASICTYPE || symtype->kind == POINTERSYM)
      tok->datatype = symtype->basicdt;
  }

  return tok;
}

TOKEN varid(TOKEN id) {
  /* Check if this variable is in the symbol table */
  return getid(id);
}

TOKEN forloop(TOKEN fortok, TOKEN varid, TOKEN assign, TOKEN assign_expression, TOKEN smash, TOKEN to_expression, TOKEN statement) {
  TOKEN ret = fortok;
  ret = makeprogn(ret, binop(assign, varid, assign_expression));

  TOKEN next = ret->operands;
  TOKEN iftok, gototok, ifexpr, varidcopy, inctok, plustok;

  /* Add the goto label */
  next->link = label(labelnumber);
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
  next->link = makegoto(labelnumber);

  return ret;
}

TOKEN function(TOKEN id, TOKEN smash, TOKEN args) {
  TOKEN ret = createtok(OPERATOR,FUNCALLOP);
  id = getid(id);
  ret->operands = id;
  (ret->operands)->link = args;

  return ret;
}

TOKEN label(int label) {
  TOKEN ret, labeltok;
  ret = talloc();
  ret->tokentype = OPERATOR;
  ret->whichval = LABELOP;
  labeltok = constant(label);
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
  labelnumber++;

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
