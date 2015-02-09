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
#include <stdlib.h>

#define MAX_STRING_LEN 15
#define NUM_RESERVED_WORDS 29
#define NUM_OPERATORS 6
#define NUM_SPECIAL_OPERATORS 13
#define NUM_DELIMETERS 8
#define MAX_NUM_SIZE 300
#define MAX_DIGITS 8

void getWholeString(char * buf, int len, bool worded);
bool isWhiteSpace(char c);
int getInt();
void consumeToBlank();
void consumeString();
bool isEndNum(int c);
void buildNumber(char * numArr, int * trailDigs, int * leadingDigs);
void buildExponent(char * expArr);

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
  }

/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
  { 
    int i;
    int c;
    for(i = 0; i < 16; i++) {
      c = peeknchar(i + 1);
      if(CHARCLASS[c] != NUMERIC) {
        if((c == '.' || c == 'e') && (c = (peeknchar(i + 2))) != '.') {
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

    double base[39];
    base[0] = 1;

    int k;
    for(k = 1; k < 39; k++) {
      base[k] = base[k - 1] * 10;
    }

    char numArr[MAX_NUM_SIZE];
    char expArr[MAX_NUM_SIZE];
    int trailDigs = 0;
    int leadingDigs = 0;
    int exponent = 0;
    double finalNumber;

    buildNumber(numArr, &trailDigs, &leadingDigs);
    finalNumber = atof(numArr);
    // printf("Number is %s with %d trailing digits\nleading digits %d\n", numArr, trailDigs,leadingDigs);
    if(peekchar() == 'e') {
      buildExponent(expArr);
      exponent = atoi(expArr) - trailDigs + leadingDigs;
    }
    else exponent = exponent - trailDigs + leadingDigs;

    // printf("Exponent is %d\n",exponent);

    /* Check if the exponent is negative or not */
    if(exponent < 0) finalNumber /= base[-exponent];
    else             finalNumber *= base[exponent];

    tok->tokentype = NUMBERTOK;
    tok->datatype = REAL;
    tok->realval = finalNumber;
    return tok;
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
    while(CHARCLASS[peekchar()] == NUMERIC || CHARCLASS[peekchar()] == ALPHA && c != EOF) {
      c = getchar();
    }
  }

  void consumeString() {
    char c = peekchar();
    if(c == '\'') getchar();
    while(c != '\'' && c != EOF) {
      c = getchar();
    }
  }

  bool isEndNum(int c) {
    return isWhiteSpace(c) || c == 'e' || (CHARCLASS[c] != NUMERIC && c != '.');
  }

  void buildNumber(char * numArr, int * trailDigs, int * leadingDigs) {
    char c;
    bool sigFig = false;
    bool foundDot = false;
    bool foundDotOnce = false;
    int index = 0;
    int numDigs = 0;

    while(!isEndNum((c = peekchar()))) {
      c = getchar();
      /* Add the first 8 significant figures to the number */
      if(c != '0' && c != '.') sigFig = true;

      if(c == '.') foundDotOnce = true;

      /* Count the number of digits after the decimal point */
      if(foundDot && numDigs < MAX_DIGITS) (*trailDigs)++;
      if(c == '.' && !sigFig) foundDot = true;
      if(sigFig) {
        /* Add the . to the number but don't incement the numDigs */
        if(c != '.') numDigs++;
        if(numDigs <= MAX_DIGITS) {
          numArr[index] = c;
          index++;
        }

        /* We only have MAX DIG numbers so we need to dvide out the potential numbers after
           out truncation by subtracting leadingDigs from the exponent */
        else {
          if(!foundDotOnce) {
            (*leadingDigs)++;
            foundDotOnce = false;
          }
        }
      }
    }
    /* Null terminate the number array */
    numArr[index] = '\0';
  }

  void buildExponent(char * expArr) {
    char c;
    int index = 0;

    /* Eat the e we are gaurunteed to haves coming in */
    getchar();
    while(!isWhiteSpace((c = peekchar()))) {
      expArr[index] = getchar();
      index++;
    }
    /* Null terminate */
    expArr[index] = '\0';
  }


