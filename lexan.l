%{    /* lex2.l -- partial lex specification for Pascal    22 Jul 13 */

/* Copyright (c) 2013 Gordon S. Novak Jr. and The University of Texas at Austin
   David Parker
   dp24559
   Project 2
*/

/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>. */

/* To use:
        make lex2
        lex2
        x < 3.14            */

/* This file will work as given on a simple input such as:
   if i<j then x := 3.14

   Copy this file to lexan.l and expand it for your assignment.
   Use:
        make lexer
        lexer <graph1.pas
   */

#include <ctype.h>
#include "token.h"
#include "lexan.h"

int maketoken(int type, int which);
int install_id();
int install_fnum();
int install_int();
int yywrap();
int toInt(char * str, bool print);
int install_string();
void parseString(char * s, char * inbuf);

/* If the lex yytext buffer turns out to be too small to hold comments,
   add the following lines:
 #undef YYLMAX
 #define YYLMAX 256
 */

TOKEN yylval;     /* This is the type of the lexical analyzer result */

%}

/* regular definitions */

delim    [ \t\n]
ws       {delim}+
letter   [A-Za-z]
digit    [0-9]
id       {letter}({letter}|{digit})*
fnumber  {digit}+(\.{digit}+)?(e[+\-]?{digit}+)?
integer  {digit}*
comment  \{[^\}]*\}|\(\*[(?!\*\))]*\*\)|\(\*[^\*\)]*\*\)
string   '([^']|'{2})*'

%%

{ws}        { /* no action and no return */ }
{comment}   { /* no action and no return */ }
{string}    {return install_string();}

array     {return maketoken(RESERVED, ARRAY);}
begin     {return maketoken(RESERVED, ARRAY + 1);}
case      {return maketoken(RESERVED, ARRAY + 2);}
const     {return maketoken(RESERVED, ARRAY + 3);}
do        {return maketoken(RESERVED, ARRAY + 4);}
downto    {return maketoken(RESERVED, ARRAY + 5);}
else      {return maketoken(RESERVED, ARRAY + 6);}
end       {return maketoken(RESERVED, ARRAY + 7);}
file      {return maketoken(RESERVED, ARRAY + 8);}
for       {return maketoken(RESERVED, ARRAY + 9);}
function  {return maketoken(RESERVED, ARRAY + 10);}
goto      {return maketoken(RESERVED, ARRAY + 11);}
if        {return maketoken(RESERVED, ARRAY + 12);}
label     {return maketoken(RESERVED, ARRAY + 13);}
nil       {return maketoken(RESERVED, ARRAY + 14);}
of        {return maketoken(RESERVED, ARRAY + 15);}
packed    {return maketoken(RESERVED, ARRAY + 16);}
procedure {return maketoken(RESERVED, ARRAY + 17);}
program   {return maketoken(RESERVED, ARRAY + 18);}
record    {return maketoken(RESERVED, ARRAY + 19);}
repeat    {return maketoken(RESERVED, ARRAY + 20);}
set       {return maketoken(RESERVED, ARRAY + 21);}
then      {return maketoken(RESERVED, ARRAY + 22);}
to        {return maketoken(RESERVED, ARRAY + 23);}
type      {return maketoken(RESERVED, ARRAY + 24);}
until     {return maketoken(RESERVED, ARRAY + 25);}
var       {return maketoken(RESERVED, ARRAY + 26);}
while     {return maketoken(RESERVED, ARRAY + 27);}
with      {return maketoken(RESERVED, ARRAY + 28);}

and       {return maketoken(OPERATOR, AND);}
or        {return maketoken(OPERATOR, OR);}
not       {return maketoken(OPERATOR, NOT);}
div       {return maketoken(OPERATOR, DIV);}
mod       {return maketoken(OPERATOR, MOD);}
in        {return maketoken(OPERATOR, IN);}
{id}      {return(install_id());}
{integer} {return(install_int());}
{fnumber} {return(install_fnum());}
"+"       {return(maketoken(OPERATOR, PLUS));}
"-"       {return(maketoken(OPERATOR, MINUS));}
"*"       {return(maketoken(OPERATOR, TIMES));}
"/"       {return(maketoken(OPERATOR, DIVIDE));}
"<"       {return(maketoken(OPERATOR, LT));}
"<="      {return(maketoken(OPERATOR, LE));}
"="       {return(maketoken(OPERATOR, EQ));}
"<>"      {return(maketoken(OPERATOR, NE));}
">"       {return(maketoken(OPERATOR, GT));}
">="      {return(maketoken(OPERATOR, GE));}
":="      {return(maketoken(OPERATOR, ASSIGN));}
"^"       {return(maketoken(OPERATOR, POINT));}
"."       {return(maketoken(OPERATOR, DOT));}

","       {return(maketoken(DELIMITER, COMMA));}
";"       {return(maketoken(DELIMITER, SEMICOLON));}
":"       {return(maketoken(DELIMITER, COLON));}
"("       {return(maketoken(DELIMITER, LPAREN));}
")"       {return(maketoken(DELIMITER, RPAREN));}
"["       {return(maketoken(DELIMITER, LBRACKET));}
"]"       {return(maketoken(DELIMITER, RBRACKET));}
".."      {return(maketoken(DELIMITER, DOTDOT));}

%%

/* Note that lex always returns two values:
   1. "What is it?": a small integer such as NUMBER that indicates the
      kind of item that was found; this is the return() value.
   2. The value itself.  This is yylval, which points to a TOKEN.   */

/* Make simple tokens. */
int maketoken(int type, int which) {
  yylval = (TOKEN) talloc();  /* Make new token, set yylval to point to it */
  yylval->tokentype = type;
  if (type == RESERVED)
     yylval->whichval = which - RESERVED_BIAS;
     else if (type == OPERATOR)
             yylval->whichval = which - OPERATOR_BIAS;
             else if (type == DELIMITER)
                     yylval->whichval = which - DELIMITER_BIAS;
                     else yylval->whichval = which;
  return(which);
}

int install_id() { 
  int i, c, n;
  yylval = (TOKEN) talloc();  /* Make new token, set yylval to point to it */
  yylval->tokentype = IDENTIFIERTOK;
  n = yyleng;            /* number of characters */
  if ( n > 15 ) n = 15;
  for (i = 0; i < n ; i++)
   { c = yytext[i];
          /* if (islower(c)) c = toupper(c); */ /* to raise to upper case */
     yylval->stringval[i] = c;
    };
  yylval->stringval[n] = '\0';
          /* printf("id  %10s   n = %4d\n", yytext, n); */
  return(IDENTIFIER);}

int install_fnum() {  /* Note -- you may use sscanf for this assignment. */
  float fnum;
  yylval = (TOKEN) talloc();   /* Make new token, set yylval to point to it */
  yylval->tokentype = NUMBERTOK;
  yylval->datatype = REAL;
       /* printf("num %10s   n = %4d\n",yytext,yyleng); */
  sscanf(yytext, "%f", &fnum);

  /* Check for floating point overflow */
  if(fnum > MAX_FLOAT || fnum < MIN_FLOAT) {
    printf("Floating number out of range\n");
    yylval->tokentype = NUMBERTOK;
    yylval->datatype = REAL;
    yylval->realval = 0.0;
    return NUMBER;
  }

  yylval->realval = fnum;
  return(NUMBER);
}

int install_int() {
  int num;
  yylval = (TOKEN) talloc();
  yylval->tokentype = NUMBERTOK;
  yylval->datatype = INTEGER;
  num = toInt(yytext, true);
  yylval->intval = num;
  return NUMBER;
}

int install_string() {
  yylval = (TOKEN) talloc();
  yylval->tokentype = STRINGTOK;
  yylval->datatype = STRING;
  parseString(yytext, yylval->stringval);
  return STRING;
}

int yywrap() {return(1);}       /* lex may need this. */

/* Converts a string to an int */
int toInt(char * str, bool print) {
  int num = 0;
  long long overflow = 0;
  int i;
  bool sign = (str[0] == '-' ? NEG : POS);

  for(i = 0; i < strlen(str); i++) {
    if( (str[i] - '0') >= 0 && (str[i] - '0') <= 9) {
      overflow = (long long)num * 10 + (str[i] - '0');
      /* Check overflow and underflow */
      if(overflow > MAX_INT || overflow < MIN_INT){
        if(print)
          printf("Integer number out of range\n"); 
        break;
     }
      num = num * 10 + (str[i] - '0');
    }
  }

  if(sign == NEG) return -num;
  return num;
}

/* Regex grabs the entire string, we just want the first 15 characters */
void parseString(char * s, char * inbuf) {
  char buf[16];
  int index = 0;
  while(*s != '\0' && index < 15) {
    if(*s != '\'') {
      buf[index] = *s;
      index++;
    }
    else {
      if(*(s+1) == '\'') {
        buf[index] = *s;
        index++;
      }
    }
    s++;
  }
  buf[index] = '\0';

  strcpy(inbuf, buf);
}

