/* codgen.c       Generate Assembly Code for x86         15 May 13   */

/* Copyright (c) 2013 Gordon S. Novak Jr. and The University of Texas at Austin
    */

/* Starter file for CS 375 Code Generation assignment.           */
/* Written by Gordon S. Novak Jr.                  */

/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License (file gpl.text) for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include "token.h"
#include "symtab.h"
#include "genasm.h"
#include "codegen.h"

void genc(TOKEN code);
/* RMIN -> RMAX (int  regs) FMIN -> FMAX (float registers) */
bool registers[FMAX + 1];

/* Set DEBUGGEN to 1 for debug printouts of code generation */
#define DEBUGGEN 0

int nextlabel;    /* Next available label number */
int stkframesize;   /* total stack frame size */

/* Top-level entry for code generator.
   pcode    = pointer to code:  (program foo (output) (progn ...))
   varsize  = size of local storage in bytes
   maxlabel = maximum label number used so far

Add this line to the end of your main program:
    gencode(parseresult, blockoffs[blocknumber], labelnumber);
The generated code is printed out; use a text editor to extract it for
your .s file.
         */

void gencode(TOKEN pcode, int varsize, int maxlabel)
  {  TOKEN name, code;
     name = pcode->operands;
     code = name->link->link;
     nextlabel = maxlabel + 1;
     stkframesize = asmentry(name->stringval,varsize);
     genc(code);
     asmexit(name->stringval);
  }

/* Trivial version: always returns RBASE + 0 */
/* Get a register.   */
/* Need a type parameter or two versions for INTEGER or REAL */
int getreg(int kind)
  {
      /* Find if we need 32 bit or 64 bit register */
      /* Find first available register mark it used and return index */
    int i;
    if(kind == INTEGER || kind == BOOLETYPE) {
      for(i = RBASE; i < RMAX; i++) {
        if(registers[i] == false) {
          registers[i] = true;
          return i;
        }
      }
      goto fail;
    }
    else if(kind == REAL || kind == STRINGTYPE || kind == POINTER) {
      for(i = FBASE; i < FMAX; i++) {
        if(registers[i] == false) {
          registers[i] = true;
          return i;
        }
      }
      goto fail;
    }
    else {
      assert(false && "Oops! Wrong kind passed in, exiting program.");
    }
    fail:
      assert(false && "Oops! Out of registers, exiting program.");
  }

void freeReg(int reg) {
  if(!registers[reg]) {
    fprintf(stderr, "Trying to free an already free'd register. %s register #%d", ((reg <= RMAX) ? "INT" : "FP"), reg);
    assert(registers[reg]);
  }

  registers[reg] = false;
}

/* Trivial version */
/* Generate code for arithmetic expression, return a register number */
int genarith(TOKEN code) {   
  int num, reg,reg2;
  double fnum;
  TOKEN lhs,rhs;
  if (DEBUGGEN) { 
    printf("genarith\n");
    dbugprinttok(code);
  };

  switch (code->tokentype) {
    case NUMBERTOK:
      switch (code->datatype) {
        case INTEGER:
          num = code->intval;
          reg = getreg(INTEGER);
          if (num >= MINIMMEDIATE && num <= MAXIMMEDIATE)
            asmimmed(MOVL, num, reg);
          break;
        case REAL:
           fnum = code->realval;
           reg = getreg(REAL);
           // if (fnum >= MINIMMEDIATE && fnum <= MAXIMMEDIATE)
           makeflit(fnum,nextlabel);
           asmldflit(MOVSD, nextlabel++, reg);
          // exit(-1);
            // asmimmed(MOVSD, fnum, reg);
          /*     ***** fix this *****   */
          break;
      }
      break;
    case IDENTIFIERTOK:
    {
      SYMBOL sym = searchst(code->stringval);
      int offset = sym->offset - stkframesize;
      switch (code->datatype) {          /* store value into lhs  */
        case INTEGER:
          reg = getreg(INTEGER);
          asmld(MOVL, offset, reg, sym->namestring);
          break;
        case REAL:
          reg = getreg(REAL);
          asmld(MOVSD, offset, reg, sym->namestring);
          break;
         /* ...  */
     };
    }
    /*     ***** fix this *****   */
      break;
    case STRINGTOK:
        /* Always EDI? */
        reg = getreg(STRINGTYPE);
        asmlitarg(nextlabel, EDI);
        makeblit(code->stringval, nextlabel++);
      break;
    case OPERATOR:
    /*     ***** fix this *****   */
      lhs = code->operands;
      rhs = lhs->link;
      reg = genarith(lhs);
      reg2 = genarith(rhs);

      if(code->whichval == PLUSOP) {
       asmrr(ADDL, reg2, reg);
      }

      freeReg(reg2);
      break;
  };
  return reg;
}

/* Generate code for a Statement from an intermediate-code form */
void genc(TOKEN code) {  
 TOKEN tok, lhs, rhs;
 int reg, offs, reg2, extra;
 SYMBOL sym;
 if(DEBUGGEN) { 
   printf("genc\n");
   dbugprinttok(code);
 };
 if(code->tokentype != OPERATOR) { 
   printf("Bad code token");
         dbugprinttok(code);
       };
 switch ( code->whichval ) { 

   case PROGNOP:
     tok = code->operands;
     while ( tok != NULL ) {  
      // fprintf(stderr,"HERE\n");
       genc(tok);
       tok = tok->link;
     };
     break;

   case ASSIGNOP:                   /* Trivial version: handles I := e */
     lhs = code->operands;
     rhs = lhs->link;
     reg = genarith(rhs);              /* generate rhs into a register */
     sym = lhs->symentry;              /* assumes lhs is a simple var  */
     offs = sym->offset - stkframesize; /* net offset of the var   */
     fprintf(stderr,"Offs is %d\n", offs);
     switch (lhs->datatype) {          /* store value into lhs  */
        case INTEGER:
          asmst(MOVL, reg, offs, lhs->stringval);
          break;
        case REAL:
          asmst(MOVSD, reg, offs, lhs->stringval);
          break;
         /* ...  */
     };
     /* HERE */
     freeReg(reg);
     if(code->link->whichval == GOTOOP) fprintf(stderr,"GOTO OP!\n");
     break;

    case LABELOP:
      asmlabel(code->datatype);
      break;

    case GOTOOP:
      fprintf(stderr,"GOTO OP!\n");
      asmjump(0, code->datatype);
      break;

    case IFOP:
      code = code->operands;
      lhs = code->operands;
      rhs = lhs->link;
      reg = genarith(lhs);
      reg2 = genarith(rhs);

      asmrr(CMPL,reg2,reg);
      freeReg(reg);
      freeReg(reg2);

      asmjump(getop(code->whichval), nextlabel);
      asmjump(0, nextlabel+1);

      asmlabel(nextlabel++);
      /* Reserve the next label */
      extra = nextlabel++;

      genc(code->link);
      asmlabel(extra);
      break;
    case FUNCALLOP:
      /* Call genarith for our arguments */
      tok = code->operands->link;
      reg = genarith(tok);

      switch(tok->datatype) {
        case INTEGER:
          asmrr(MOVL, reg, EDI);
          break;
      }

      freeReg(reg);

      asmcall(code->operands->stringval);
      break;
 };
}