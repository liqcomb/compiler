-- Tiger language lexer using Alex
-- References: 
-- [1] https://cs.nyu.edu/courses/fall13/CSCI-GA.2130-001/tiger-spec.pdf
-- [2] https://www.haskell.org/alex/doc/alex.pdf
{
{-# LANGUAGE OverloadedStrings #-}
module Lexer (
  Token(..), Alex(..),
  tokenize, lexer, runAlex
) where

import Data.Char (chr, ord)
}

-- Wrapper type, use standard alex monad wrapper here
%wrapper "monadUserState"

-- Character set macros
$digits = [0-9]
$newline = [\n\r\f]
$whitechar = [\v\ ]
$tab = \t
$colon = :
$seq = \;

-- Regex macros
@identifier = [A-Za-z_][0-9A-Za-z_]*

-- Rules
tiger :-

<0> { 
  $whitechar+ ;   -- matching whitespaces & tabs
  $tab+ ;
  $newline+ ;

  "type" { mkT Ttype }  -- matching keywords & symbols in state 0
  "array" { mkT Tarray }
  "of" { mkT Tof }
  "var" { mkT Tvar }
  "function" { mkT Tfunction }
  "in" { mkT Tin }
  "if" { mkT Tif }
  "then" { mkT Tthen } 
  "else" { mkT Telse }
  "for" { mkT Tfor }
  "to" { mkT Tto }
  "do" { mkT Tdo }
  "break" { mkT Tbreak }
  "let" { mkT Tlet }
  "end" { mkT Tend }
  "while" { mkT Twhile }
  "nil" { mkT Tnil }
  ":=" { mkT Tassign }
  "+" { mkT Tplus }
  "-" { mkT Tminus }
  "*" { mkT Tasterisk }
  "/" { mkT Tslash }
  ":" { mkT Tcolon }
  "=" { mkT Tequal }
  "<>" { mkT Tnequal }
  "&" { mkT Tampersand }
  "|" { mkT Tvbar }
  ">" { mkT Tgreater }
  ">=" { mkT TgreaterE }
  "<" { mkT Tless }
  "<=" { mkT TlessE }
  ";" { mkT Tsemicolon }
  "(" { mkT Tlparen }
  ")" { mkT Trparen }
  "[" { mkT Tlbracket }
  "]" { mkT Trbracket }
  \{ { mkT Tlcurly }
  \} { mkT Trcurly }
  \, { mkT Tcomma }
  \. { mkT Tdot }

  \" { beginString } -- matching string literal
  @identifier { mkId } -- matching identifier
  $digits+ { mkInt }  -- matching int literal
}
  
<0,comment> "/*" { beginComment }  -- matching comment

<comment> {
  "*/" { endComment }
  [.\n] ;
}

<stringLit> {
  \" { endString }
  \\([nt\"\\]|$digits{3})  { escapeString }
  .  { appendString }
}

{

mkInt :: AlexAction Token
mkInt (_,_,_,s) len = return $ TintLiteral $ read $ take len s

mkId :: AlexInput -> Int -> Alex Token
mkId (_,_,_,s) len = return $ Tid $ take len s

mkT :: Token -> AlexInput -> Int -> Alex Token
mkT t _ _ = return t

-- Identifiers including varnames/typenames/function names
type Identifier = String

data Token
  = Ttype     -- keywords
  | Tarray
  | Tof
  | Tvar
  | Tfunction
  | Tin
  | Tif
  | Tthen
  | Telse
  | Tfor
  | Tto
  | Tdo
  | Tbreak
  | Tlet
  | Tend
  | Twhile
  | Tnil
  | Tplus   -- symbols
  | Tminus
  | Tasterisk
  | Tslash
  | Tcolon
  | Tsemicolon
  | Tequal
  | Tampersand
  | Tvbar
  | Tnequal
  | Tgreater
  | TgreaterE
  | Tless
  | TlessE
  | Tassign 
  | Tlparen
  | Trparen
  | Tlbracket
  | Trbracket
  | Tlcurly
  | Trcurly
  | Tcomma
  | Tdot
  | Tid Identifier  -- identifiers & literals
  | TintLiteral Int
  | TstringLiteral String
  | Teof  -- EOF sign
  | Tdummy  -- dummy token
  deriving (Show, Ord, Eq)

alexEOF :: Alex Token
alexEOF = return Teof

data AlexUserState = AlexUserState {
  string_buf :: String,
  escaped :: Bool,
  comment_depth :: Int
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { string_buf = "", escaped = False, comment_depth = 0 }

getCommentDepth :: Alex Int
getCommentDepth = Alex $ \s@AlexState {alex_ust=ust} -> Right (s, comment_depth ust)

setCommentDepth :: Int -> Alex ()
setCommentDepth c = Alex $ \s -> Right (s {alex_ust = (alex_ust s) { comment_depth = c } }, ())

getStringBuf :: Alex String
getStringBuf = Alex $ \s@AlexState {alex_ust=ust} -> Right (s, string_buf ust)

setStringBuf :: String -> Alex ()
setStringBuf c = Alex $ \s -> Right (s {alex_ust = (alex_ust s) { string_buf = c } }, ())

appendStringBuf :: String -> Alex ()
appendStringBuf s = do
  b <- getStringBuf
  setStringBuf (b ++ s)

getEscaped :: Alex Bool
getEscaped = Alex $ \s@AlexState {alex_ust=ust} -> Right (s, escaped ust)

setEscaped :: Bool -> Alex ()
setEscaped c = Alex $ \s -> Right (s {alex_ust = (alex_ust s) { escaped = c } }, ())

beginComment :: AlexInput -> Int -> Alex Token
beginComment a b = do
  depth <- getCommentDepth
  setCommentDepth (depth + 1) >> begin comment a b

endComment :: AlexInput -> Int -> Alex Token
endComment a b = do
  depth <- getCommentDepth
  let code = if depth == 1 then 0 else comment
  setCommentDepth (depth - 1) >> begin code a b

beginString :: AlexAction Token
beginString = begin stringLit

endString :: AlexAction Token
endString _ _ = do
  s <- getStringBuf
  setStringBuf "" >> alexSetStartCode 0
  return $ TstringLiteral s

octToInt :: String -> Int
octToInt x = fromOct x 0
  where fromOct (x:_) v = ((v * 8) + (ord x) - 0x30)
        fromOct [] v = v

escapeString :: AlexInput -> Int -> Alex Token
escapeString a@(_,_,_,s) b = do
  let ctnt =
        case (s !! 1) of
          'n' -> "\n"
          't' -> "\t"
          '"' -> "\""
          '\\' -> "\\"
          _ -> [chr $ octToInt $ take 3 $ tail s]
  appendStringBuf ctnt >> skip a b

appendString :: AlexInput -> Int -> Alex Token
appendString a@(_,_,_,s) b = do
  appendStringBuf [s !! 0] >> skip a b

-- gather tokens
doTokenize :: Alex [Token]
doTokenize = getNext Tdummy []
  where
    getNext e l =
      case e of
        Teof -> return l
        _ -> do
          t <- alexMonadScan
          getNext t (l ++ [t])

-- Debug-purpose function
tokenize :: String -> Either String [Token]
tokenize s = runAlex s doTokenize

-- Exposed interface for parser
lexer :: (Token -> Alex a) -> Alex a
lexer cont = do
  tk <- alexMonadScan
  cont tk

}
