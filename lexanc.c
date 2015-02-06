/* lex1.c         14 Feb 01; 31 May 12       */

/* This file contains code stubs for the lexical analyzer.
   Rename this file to be lexanc.c and fill in the stubs.    */

/* Copyright (c) 2001 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include "token.h"
#include "lexan.h"

/* This file will work as given with an input file consisting only
   of integers separated by blanks:
   make lex1
   lex1
   12345 123    345  357
   */

/* Skip blanks and whitespace.  Expand this function to skip comments too. */
void skipblanks ()
  {
      int c;
      while ((c = peekchar()) != EOF
             && (c == ' ' || c == '\n' || c == '\t'))
          getchar();

      /* Consume the { } version of comments -- doesn't handles nesting */
      const char OPEN_COMM = '{';
      const char CLOSE_COMM = '}';
      const char OPEN1_COMM = '(';
      const char STAR_COMM = '*';
      const char CLOSE1_COMM = ')';
      bool comment = false;

      if(peekchar() == OPEN_COMM) comment = true;

      while ((c = peekchar()) != EOF && comment) {
        if(c == CLOSE_COMM) {
          comment = false;
          if((peekchar() != OPEN1_COMM && peek2char() != STAR_COMM) && peekchar() != OPEN_COMM)
            getchar();
          skipblanks();
        }
        else getchar();
      }

      /* Consumes the (* *) version of comments */
      comment = false;

      if(peekchar() == OPEN1_COMM && peek2char() == STAR_COMM) comment = true;

      while((c = peekchar()) != EOF && comment) {
        getchar();
        if(c == STAR_COMM && peekchar() == CLOSE1_COMM) {
          comment = false;
          if((peekchar() != OPEN1_COMM && peek2char() != STAR_COMM) && peekchar() != OPEN_COMM)
            getchar();
          skipblanks();
        }
      }
  }

/* Get identifiers and reserved words */
TOKEN identifier (TOKEN tok)
  {
    printf("Called identifier");
    }

TOKEN getstring (TOKEN tok)
  {
    /* Consume the single quote that we peeked */
    getchar();

    int i;
    char c;
    for(i = 0; i < 16; i++) {
      c = peekchar();
      if(c == EOF) break;
      else if(c == '\'') {
        if(peek2char() == '\'')  {
          tok->stringval[i] = c;
          getchar();
        }
        else {
          getchar();
          break;
        }
      }
      else {
        tok->stringval[i] = c;
      }
      getchar();
    }

    tok->tokentype = STRINGTOK;
    tok->stringval[i] = '\0';
    tok->datatype = STRINGTYPE;
    return tok;
    }

TOKEN special (TOKEN tok)
  {
    printf("Called special");
    }

/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
  { long num;
    int  c, charval;
    num = 0;
    while ( (c = peekchar()) != EOF
            && CHARCLASS[c] == NUMERIC)
      {   c = getchar();
          charval = (c - '0');
          num = num * 10 + charval;
        }
    tok->tokentype = NUMBERTOK;
    tok->datatype = INTEGER;
    tok->intval = num;
    return (tok);
  }

