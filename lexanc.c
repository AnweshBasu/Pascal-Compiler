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

#define MAX_STRING_LEN 16
#define NUM_RESERVED_WORDS 29
#define NUM_OPERATORS 6
#define NUM_SPECIAL_OPERATORS 13
#define NUM_DELIMETERS 8

void getWholeString(char * buf, int len, bool worded);
bool isWhiteSpace(char c);
int getInt();
void consumeToBlank();
void consumeString();

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
             && isWhiteSpace(c))
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

      if(peekchar() == OPEN1_COMM && peek2char() == STAR_COMM){ comment = true; getchar(); getchar();}

      while((c = peekchar()) != EOF && comment) {
        c = getchar();
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
    /* First check for a reserved word */
    char string[MAX_STRING_LEN];
    getWholeString(string,MAX_STRING_LEN,true);

    const char* operators[NUM_OPERATORS] = {"and", "or", "not", "div", "mod", "in"};

    const char* rWords[NUM_RESERVED_WORDS] = 
                                 {"array", "begin", "case", "const", "do",
                                  "downto", "else", "end", "file", "for",
                                  "function", "goto", "if", "label", "nil",
                                  "of", "packed", "procedure", "program", "record",
                                  "repeat", "set", "then", "to", "type",
                                  "until", "var", "while", "with"
                                  };

    int i;

    /* Check for reserved words */
    for(i = 0; i < NUM_RESERVED_WORDS; i++) {
      /* We found a reserved word */
      if(strcmp(string, rWords[i]) == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = i + 1;
        /* TODO Verify that this is an integer type */
        return tok;
      }
    }

    /* Check for worded operators */
    for(i = 0; i < NUM_OPERATORS; i++) {
      if(strcmp(string, operators[i]) == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = i + (OR - OPERATOR_BIAS) - 1;
        return tok;
      }
    }

    /* Just a user defined identifier */
    tok->tokentype = IDENTIFIERTOK;
    strcpy(tok->stringval,string);
    tok->datatype = STRINGTYPE;
    consumeToBlank();
    return tok;
  }

TOKEN getstring (TOKEN tok)
  {
    /* Consume the single quote that we peeked */
    getchar();

    int i;
    char c;
    for(i = 0; i < 15; i++) {
      c = peekchar();
      if(c == EOF) break;
      else if(c == '\'') {
        if(peek2char() == '\'')  {
          tok->stringval[i] = c;
          getchar();
        }
        else {
          //getchar();
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
    consumeString();
    return tok;
  }

TOKEN special (TOKEN tok)
  {
    const char* specialOps[NUM_SPECIAL_OPERATORS] = {"+","-","*","/",":=","=","<>","<",
                                      "<=",">=",">","^","."};

    const char* delimeters[NUM_DELIMETERS] = {",",";",":","(",")","[","]",".."};

    char sToken[3];
    getWholeString(sToken,3,false);

    /* Delimeters */
    int i;
    for(i = 0; i < NUM_DELIMETERS; i++) {
      if(strcmp(sToken,delimeters[i]) == 0) {
        tok->tokentype = DELIMITER;
        tok->whichval = i + 1;
        return tok;
      }
    }

    /* Operators */
    for(i = 0; i < NUM_SPECIAL_OPERATORS; i++) {
      if(strcmp(sToken,specialOps[i]) == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = i + 1;
        return tok;
      }
    }

    //printf("I'm Helping: No token found\n");
  }

/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
  { 
    int i;
    int c;
    for(i = 0; i < 16; i++) {
      c = peeknchar(i + 1);
      if(CHARCLASS[c] != NUMERIC || i == 15) {
        // printf("c is %c\n", c);
        if(c == '.' || c == 'e') {
          break;
        }
        else {
          int ret = getInt();
          tok->tokentype = NUMBERTOK;
          tok->datatype = INTEGER;
          tok->intval = ret;
          consumeToBlank();
          return tok;
        }
      }
    }

    /* Floating point */
    char wholeNumStr[16];
    char EvalStr[16];
    double wholeNum;
    int decPlace = 0;
    int Eval = 1;
    int base[16] = {1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000,1000000000,
                    1000000000,1000000000,1000000000,1000000000,1000000000};

    for(i = 0; i < 16; i++) {
      c = peekchar();
      if(CHARCLASS[c] != NUMERIC) {
        // printf("Found a non numeric: %c\n", c);
       // c = getchar();
        if(c == '.') {
          //printf("found .\n");
          decPlace = i;
          i--;
        }
        else if(c == 'e') {
          // printf("found e\n");
          int j;
          getchar();
          for(j = 0; j < 16; j++) {
            c = peekchar();
            if(CHARCLASS[c] != NUMERIC) {
              if(c == '+' || c == '-') {
                EvalStr[j] = getchar();
              }
              else {
                // printf("DONE\n");
                /* return the final value here */
                wholeNumStr[i] = '\0';
                EvalStr[j] = '\0';
                wholeNum = atoi(wholeNumStr);
                Eval = atoi(EvalStr);
                decPlace = decPlace - strlen(wholeNumStr);
                // printf("Decimal Place: %d\n", decPlace);
                Eval = Eval + decPlace;
                // printf("Eval: %s\n", EvalStr);

                if(Eval < 0)
                  wholeNum = (double)wholeNum / (double)base[-Eval];
                else 
                  wholeNum = (double)wholeNum * (double)base[Eval];

                tok->tokentype = NUMBERTOK;
                tok->datatype = REAL;
                tok->realval = wholeNum;
                consumeToBlank();
                return tok;
              }
            }

            else {
              EvalStr[j] = getchar();
            }
          }
        }
        else {
          // No e
          wholeNumStr[i] = '\0';
          // printf("Dec place = %d\nWhole String = %s\ni = %d\n", decPlace, wholeNumStr,i);
          decPlace = decPlace - strlen(wholeNumStr);
          if(decPlace < 0)
            wholeNum = (double)atoi(wholeNumStr) / (double)base[-decPlace];
          else
            wholeNum = (double)atoi(wholeNumStr) * (double)base[decPlace];

          tok->tokentype = NUMBERTOK;
          tok->datatype = REAL;
          tok->realval = wholeNum;
          consumeToBlank();
          return tok;
        }
      }
      else {
        wholeNumStr[i] = c;
      }
      getchar();
    }
    /*ERROR*/
    printf("Shoudln't be here\n");

  }

  void getWholeString(char * buf, int len, bool worded) {
    // printf("Len: %d\n", len);
    int i;
    char c, cclass;
    for(i = 0; i < len; i++) {
      c = peekchar();
      cclass = CHARCLASS[c];
      if(worded) {
        if(cclass == ALPHA || cclass == NUMERIC)
          buf[i] = getchar();
        else break;
      }
      /* Non word or number types */
      else {
        /* Handle cases where two delimiters might be next to each other, basically only add one character 
           into the string except for the special cases of 2 character tokens, and just pass a string buffer
           large enough to store 2 characters and a null terminator */
        if(cclass != ALPHA && cclass != NUMERIC && c!= EOF && !isWhiteSpace(c)) {
          buf[i] = getchar();
          if(buf[i] == ':' && peekchar() == '=')
            buf[++i] = getchar();
          else if(buf[i] == '<' && (peekchar() == '>' || peekchar() == '='))
            buf[++i] = getchar();
          else if(buf[i] == '>' && peekchar() == '=')
            buf[++i] = getchar();
          else if(buf[i] == '.' && peekchar() == '.')
            buf[++i] = getchar();
          i++;
          break;
        }
        else break;
      }
    }
    buf[i] = '\0';
     // printf("i = %d, obtained string: %s\n",i, buf);
  }

  bool isWhiteSpace(char c) {
    return (c == ' ' || c == '\n' || c == '\t');
  }

  int getInt() {
    long num;
    int  c, charval;
    num = 0;
    while ( (c = peekchar()) != EOF && CHARCLASS[c] == NUMERIC) {   
      c = getchar();
      charval = (c - '0');
      num = num * 10 + charval;
    }

    return num;
  }

  void consumeToBlank() {
    char c = peekchar();
    //while(!isWhiteSpace(peekchar()) && peekchar() != EOF && peekchar() != ';') {
    while(CHARCLASS[peekchar()] == NUMERIC || CHARCLASS[peekchar()] == ALPHA && c != EOF) {
      c = getchar();
     // printf("C blank is %c\n", c);
      //c = peekchar();
    }
  }

  void consumeString() {
    char c = peekchar();
    if(c == '\'') getchar();
    //printf("Peek char is %c\n", c);
    while(c != '\'' && c != EOF) {
      c = getchar();
      //printf("C string is %c\n", c);
      //c = peekchar();
    }
  }


