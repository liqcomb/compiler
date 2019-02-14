{-# LANGUAGE OverloadedStrings #-}

module Absyn where

type Identifier = String

-- https://cs.nyu.edu/courses/fall13/CSCI-GA.2130-001/tiger-spec.pdf
-- The whole program
data Program =
  Program Expression
  deriving (Show)

-- Declaration
data Declaration =
  TypeDec Identifier Type
  | VarDec Variable
  | FunDec Function
  deriving (Show)

-- Type declaration
data Type =
  Type Identifier
  | Array Identifier
  | Record [FieldDeclaration]
  deriving (Show)

-- Record field declaration
data FieldDeclaration =
  FieldDeclaration Identifier Identifier
  deriving (Show)

-- Function declaration
data Function =
  Function Identifier [FieldDeclaration] Expression
  | TypedFunction Identifier [FieldDeclaration] Identifier Expression
  deriving (Show)

-- Variable declaration
data Variable =
  Variable Identifier Expression
  | TypedVariable Identifier Identifier Expression
  deriving (Show)

-- LHS value
data LValue =
  LId Identifier
  | LSubscript LValue Expression
  | LFieldExp LValue Identifier
  deriving (Show)

-- Field creation
data FieldCreate =
  FieldCreate Identifier Expression
  deriving (Show)

-- Infix operations
data InfixOp =
  Add
  | Sub
  | Multiply
  | Divide
  | Equal
  | NotEqual
  | Greater
  | Less
  | GreaterE
  | LessE
  | And
  | Or
  | Assign
  deriving (Show, Eq)

-- A single expression
data Expression =
  Value LValue
  | Nil
  | IntLit Int
  | StringLit String
  | SeqExp [Expression]
  | Negation Expression
  | Call Identifier [Expression]
  | Infix Expression InfixOp Expression
  | ArrCreate Identifier Expression Expression
  | RecCreate Identifier [FieldCreate]
  | Assignment LValue Expression
  | IfThenElse Expression Expression Expression
  | IfThen Expression Expression
  | While Expression Expression
  | ForTo Identifier Expression Expression Expression
  | Break
  | LetIn [Declaration] [Expression]
  deriving (Show)
