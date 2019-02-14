-- Tiger language parser using happy.
{
{-# LANGUAGE OverloadedStrings #-}
module Parser (
  Program(..), Declaration(..), Type(..), FieldDeclaration(..), Function(..), Variable(..), LValue(..), FieldCreate(..), Expression(..), InfixOp(..),
  parse
) where

import Lexer
import Absyn
import Control.Monad.Trans (liftIO)
}

%token
 'type' { Ttype }   -- keywords
 'array' { Tarray }
 'of' { Tof }
 'var' { Tvar }
 'function' { Tfunction }
 'in' { Tin }
 'if' { Tif }
 'then' { Tthen }
 'else' { Telse }
 'for' { Tfor }
 'to' { Tto }
 'do' { Tdo }
 'break' { Tbreak }
 'let' { Tlet }
 'end' { Tend }
 'while' { Twhile }
 'nil' { Tnil }
 ':=' { Tassign }
 '+' { Tplus }
 '-' { Tminus }
 '*' { Tasterisk }
 '/' { Tslash }
 ':' { Tcolon }
 '=' { Tequal }
 '<>' { Tnequal }
 '&' { Tampersand }
 '|' { Tvbar }
 '>' { Tgreater }
 '>=' { TgreaterE }
 '<' { Tless }
 '<=' { TlessE }
 ';' { Tsemicolon }
 '(' { Tlparen }
 ')' { Trparen }
 '[' { Tlbracket }
 ']' { Trbracket }
 '{' { Tlcurly }
 '}' { Trcurly }
 ',' { Tcomma }
 '.' { Tdot }
 ident { Tid $$ }
 strLit { TstringLiteral $$ }
 intLit { TintLiteral $$ }

%tokentype { Token }

%name tigerParse Program
%error { tigerError }
%monad { Alex }
%lexer { lexer } { Teof }

%left '|'
%left '&'
%nonassoc '=' '<>' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'

%%

Program     :  Expression                       { Program $1 }

Declaration :  TypeDec                          { $1 }
            |  VarDec                           { VarDec $1 }
            |  FunDec                           { FunDec $1 }

TypeDec     :  'type' ident '=' Type            { TypeDec $2 $4 }

Type        :  'array' 'of' ident               { Array $3 }
            |  '{' FieldDecs '}'                { Record $2 }
            |  ident                            { Type $1 }


FieldDec    :  ident ':' ident                  { FieldDeclaration $1 $3 }

FieldDecs   :                                   { [] }
            |  FieldDec                         { [$1] }
            |  FieldDecs ',' FieldDec           { $1 ++ [$3] }

FunDec      :  'function' ident '(' FieldDecs ')' '=' Expression  { Function $2 $4 $7 }
            |  'function' ident '(' FieldDecs ')' ':' ident '=' Expression  { TypedFunction $2 $4 $7 $9 }

VarDec      :  'var' ident ':=' Expression      { Variable $2 $4 }
            |  'var' ident ':' ident ':=' Expression { TypedVariable $2 $4 $6 }

LValue      :  ident                            { LId $1 }
            |  LValue '[' Expression ']'        { LSubscript $1 $3 }
            |  LValue '.' ident                 { LFieldExp $1 $3 }

Expression  :  LValue                           { Value $1 }
            |  'nil'                            { Nil }
            |  intLit                           { IntLit $1 }
            |  strLit                           { StringLit $1 }
            |  '(' SeqExp ')'                   { SeqExp $2 }
            |  Negation                         { $1 }
            |  ident '(' ExpressionList ')'     { Call $1 $3 }
            |  Infix                            { $1 }
            |  ident '[' Expression ']' 'of' Expression { ArrCreate $1 $3 $6 }
            |  ident '{' FieldCreates '}'       { RecCreate $1 $3 }
            |  LValue ':=' Expression           { Assignment $1 $3 }
            |  'if' Expression 'then' Expression 'else' Expression  { IfThenElse $2 $4 $6 }
            |  'if' Expression 'then' Expression  { IfThen $2 $4 }
            |  'while' Expression 'do' Expression { While $2 $4 }
            |  'for' ident ':=' Expression 'to' Expression 'do' Expression { ForTo $2 $4 $6 $8 }
            |  'break'                          { Break }
            |  'let' DeclarationList 'in' SeqExp 'end'  { LetIn $2 $4 }

SeqExp      :                                   { [] }
            |  Expression                       { [$1] }
            |  SeqExp ';' Expression            { $1 ++ [$3] }

Negation    :  '-' Expression                   { Negation $2 } 

ExpressionList :                                { [] }
            |  Expression                       { [$1] }
            | ExpressionList ',' Expression     { $1 ++ [$3] }

Infix     :  Expression '+' Expression          { Infix $1 Add $3 }
          |  Expression '-' Expression          { Infix $1 Sub $3 }
          |  Expression '*' Expression          { Infix $1 Multiply $3 }
          |  Expression '/' Expression          { Infix $1 Divide $3 }
          |  Expression '=' Expression          { Infix $1 Equal $3 }
          |  Expression '<>' Expression         { Infix $1 NotEqual $3 }
          |  Expression '>' Expression          { Infix $1 Greater $3 }
          |  Expression '>=' Expression         { Infix $1 GreaterE $3 }
          |  Expression '<' Expression          { Infix $1 Less $3 }
          |  Expression '<=' Expression         { Infix $1 LessE $3 }
          |  Expression '&' Expression          { Infix $1 And $3 }
          |  Expression '|' Expression          { Infix $1 Or $3 }

FieldCreate   :  ident '=' Expression           { FieldCreate $1 $3 }

FieldCreates  :                                 { [] }
              |  FieldCreate                    { [$1] }
              |  FieldCreates ',' FieldCreate   { $1 ++ [$3] }

DeclarationList  :  Declaration                 { [$1] }
                 |  DeclarationList Declaration { $1 ++ [$2] }

{

-- custom error
tigerError tks = error $ "Parser error, remaining tokens: " ++ (show tks)

parse :: String -> Either String Program
parse s = runAlex s tigerParse
}
