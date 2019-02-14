{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS -Wno-name-shadowing #-}

module Semantic (
  Ty(..), newLabel, newTemp, addString, getBreakLabel, setBreakLabel, getVarRef,
  typecheck
) where

import qualified Data.Map as Map
import qualified Absyn as A
import qualified Mips as M

import Control.Monad.State
import Text.Printf (printf)
import Data.List (findIndex)

type Identifier = String
type SymbolTable = Map.Map Identifier (Identifier, Expr)  -- symbol -> type identifier
type TypeTable = Map.Map Identifier Type  -- type identifier -> type info
type Label = String   -- represents a to-be-determined memory location
type Temp = Int       -- represents a to-be-determined reg/mem for local var

data Environment = Environment {
  types :: TypeTable, 
  symbols :: SymbolTable,
  string_lit :: Map.Map String Label,
  label_idx :: Int,
  temp_idx :: Int,
  depth :: Int,
  break_label :: Label,
  functions :: Map.Map String (Stm, [Stm]),
  prologs :: [Stm]
} deriving (Show)

newtype Ty a = Ty (State Environment a) deriving
  (Functor, Applicative, Monad, MonadState Environment)

-- All types used in tiger compiler
data Type = 
  TyInt
  | TyStr
  | TyNil
  | TyArray Identifier
  | TyUnit
  | TyRecord [(String, Identifier)]
  | TyFunction Identifier [(String, Identifier)]
  | TyUnknown
  deriving (Eq, Show)

emptySymbols :: SymbolTable
emptySymbols = Map.empty

emptyTypes :: TypeTable
emptyTypes = Map.fromList [("int", TyInt), ("string", TyStr)]

emptyEnv :: Environment
emptyEnv = Environment {
  types = emptyTypes,
  symbols = emptySymbols,
  string_lit = Map.empty,
	label_idx = 0,
  temp_idx = 0,
  depth = 0,
  break_label = "exit_label",
  functions = Map.empty,
  prologs = []
}

newLabel :: Ty Label
newLabel = do
  idx <- gets label_idx
  modify $ \s -> s { label_idx = idx + 1 }
  return $ printf "_L%03d" idx

newTemp :: Ty Int
newTemp = do
  idx <- gets temp_idx
  modify $ \s -> s { temp_idx = idx + 1 }
  return idx

addString :: String -> Label -> Ty ()
addString s l = modify $ \st -> st { string_lit = Map.insert s l (string_lit st) }

addType :: Identifier -> Type -> Ty ()
addType tyId ty = do
  tytbl <- gets types
  modify $ \s -> s { types = Map.insert tyId ty tytbl }
  return ()

addVarWithType :: Identifier -> Identifier -> Ty ()
addVarWithType varId tyId = do
  syms <- gets symbols
  t <- newTemp
  modify $ \s -> s { symbols = Map.insert varId (tyId, (Temp t)) syms }
  return ()

getVarRef :: Identifier -> Ty Expr
getVarRef s = do
  syms <- gets symbols
  let (_, ref) = maybe (error "undefined symbol") id $ Map.lookup s syms
  return $ ref

tyIdToType :: Identifier -> Ty Type
tyIdToType x = do
  tbl <- gets types
  return $! maybe (error "unknown type") id (Map.lookup x tbl)

varIdToType :: Identifier -> Ty Type
varIdToType x = do
  vartbl <- gets symbols
  tbl <- gets types
  let (tyId, _) = maybe (error "undefined symbol") id (Map.lookup x vartbl)
  return $! maybe (error "unknown type") id (Map.lookup tyId tbl)

typeToTyId :: Type -> Ty Identifier
typeToTyId ty = do
  tbl <- gets types
  return $ fst $ head $ filter (\(_, typ) -> typ == ty) $ Map.toList tbl

setBreakLabel :: Label -> Ty ()
setBreakLabel l = modify $ \s -> s { break_label = l }

getBreakLabel :: Ty Label
getBreakLabel = gets break_label

-- Type checking stuff

recordOffset :: Type -> Identifier -> Int
recordOffset (TyRecord flds) fid = maybe (error "undefined field") id $ findIndex (\s -> s == fid) $ map fst flds

-- there must be a much better way to solve this...
unwrapTypes :: [Type] -> Type -> Type
unwrapTypes (x:xs) ty = 
  case x of
    TyUnknown -> error "unknown type"
    _ -> unwrapTypes xs ty
unwrapTypes [] ty = ty

checkType :: Type -> Type -> Type
checkType a b = if a == b then a else error "incompatible types"

checkInfixType :: Type -> A.InfixOp -> Type -> Type
checkInfixType TyStr A.Equal TyStr = TyInt
checkInfixType TyStr A.NotEqual TyStr = TyInt
checkInfixType TyInt _ TyInt = TyInt
checkInfixType _ _ _ = error "type does not match for infix operation"

checkArrType :: Type -> Type -> Type -> Ty Type
checkArrType a@(TyArray tyId) TyInt ivalty = do 
  ty <- tyIdToType tyId
  return $! if ty == ivalty then a else error "array type not matching"
checkArrType _ TyInt _ = do return $! error "invalid array type"
checkArrType (TyArray _) _ _ = do return $! error "invalid array size type"
checkArrType _ _ _ = do return $! error "invalid array creation types"

checkSubscriptType :: Type -> Type -> Ty Type
checkSubscriptType (TyArray tyId) TyInt = varIdToType tyId
checkSubscriptType _ TyInt = do return $! error "invalid subscript type for subscript expression"
checkSubscriptType (TyArray _) _ = do return $! error "invalid lvalue type for subscript expression"
checkSubscriptType _ _ = do return $! error "invalid types for subscripts expression"

checkFieldType :: Type -> Identifier -> Ty Type
checkFieldType (TyRecord fields) name = do
  let (_, tyId) = head $ filter (\(fname, _) -> name == fname) fields
  varIdToType tyId
checkFieldType _ _ = do return $! error "invalid lvalue type for field expression"

checkForTo :: Type -> Type -> Type
checkForTo TyInt TyInt = TyUnit
checkForTo _ _ = error "invalid for-to loop variable type"

checkRecFieldType :: [A.FieldCreate] -> (String, Identifier) -> Ty (Type, Exp)
checkRecFieldType fcs (name, typeId) = do
  let field = filter (\(A.FieldCreate fname _) -> name == fname) fcs
  if (length field) == 1
    then do
      let (A.FieldCreate fname ex) = head field
      (exty, exex) <- checkExp ex
      ty <- tyIdToType typeId
      return $! if ty == exty then (exty, exex) else error ("invalid type for field " ++ (show name))
    else return $! error "unmatched field name or ambiguous field defined"

checkRecType :: Type -> [A.FieldCreate] -> Ty (Type, Exp)
checkRecType ty@(TyRecord fields) fc = do 
  if (length fc) == (length fields)
    then do
      (tys, exs) <- fmap unzip $ mapM (checkRecFieldType fc) fields
      rtex <- record exs
      return $ (ty, rtex)
    else return $! error "incorrect numbers of fields"
checkRecType _ _ = return $! error "invalid record type identifier"

checkCallExprType :: ((String, Identifier), A.Expression) -> Ty (Type, Exp)
checkCallExprType ((_, tyId), ex) = do
  ty <- tyIdToType tyId
  (exty, exex) <- checkExp ex
  return $! if ty == exty then (exty, exex) else error "unmatched argument type"

checkCallType :: Type -> [A.Expression] -> Identifier -> Ty (Type, Exp)
checkCallType (TyFunction retTyId argTyIds) exps funcId = do
  retty <- tyIdToType retTyId
  if (length argTyIds) == (length exps)
    then do
      (_, exs) <- fmap unzip $ mapM checkCallExprType $ zip argTyIds exps
      rtex <- call funcId exs
      return $! (retty, rtex)
    else return $! error ("expecting " ++ (show $ length argTyIds) ++ " argument(s) in this function.")
checkCallType _ _ _ = do return $! error "invalid symbol type for call expression"

checkIfThenElse :: Type -> Type -> Type -> Type
checkIfThenElse _ tb fb = if tb == fb then tb else error "if branches' type not matched"

runTy :: Environment -> Ty a -> Environment
runTy env (Ty ty) = execState ty env

-- typecheck the program
typecheck :: A.Program -> Environment
typecheck (A.Program exp) = 
  runTy emptyEnv $ do
    (ty, ex) <- checkExp exp
    stm <- unNx ex
    modify $ \s -> s { functions = Map.insert "entrypoint" (stm, (prologs s)) (functions s) }

checkLVal :: A.LValue -> Ty (Type, Exp)
checkLVal = do
  \case
    A.LId varid -> do 
      ty <- varIdToType varid
      ref <- getVarRef varid
      return $ (ty, Ex $ ref)
    A.LSubscript lval index -> do
      (lvalty, lvalexp) <- checkLVal lval
      (idxty, idxexp) <- checkExp index
      ty <- checkSubscriptType lvalty idxty
      rtex <- offset lvalexp idxexp
      return $ (ty, rtex)
    A.LFieldExp lval fieldid -> do
      (lvalty, lvalexp) <- checkLVal lval
      ty <- checkFieldType lvalty fieldid
      rtex <- offset lvalexp (Ex $ Const $ recordOffset ty fieldid)
      return $ (ty, rtex)

declareType :: Identifier -> A.Type -> Ty ()
declareType tyId (A.Type tyId2) = do
  ty <- tyIdToType tyId2
  addType tyId ty
declareType tyId (A.Array tyId2) = do
  addType tyId (TyArray tyId2)
declareType tyId (A.Record fielddecs) = do
  let decs = map (\(A.FieldDeclaration name ftyId) -> (name, ftyId)) fielddecs
  addType tyId (TyRecord decs)

declareVar :: A.Variable -> Ty ()
declareVar (A.Variable varId exps) = do
  (varty, varex) <- checkExp exps
  tyId <- typeToTyId varty
  ex <- unEx varex
  _ <- addVarWithType varId tyId
  ref <- getVarRef varId
  modify $ \s -> s { prologs = (prologs s) ++ [Move ref ex] }
declareVar (A.TypedVariable varId tyId exps) = do
  (varty, varex) <- checkExp exps
  ty <- tyIdToType tyId
  ex <- unEx varex
  if ty == varty
    then do
      _ <- addVarWithType varId tyId
      ref <- getVarRef varId
      modify $ \s -> s { prologs = (prologs s) ++ [Move ref ex] }
    else return $! error "unmatched typed variable declaration"

declareFunction :: A.Function -> Ty ()
declareFunction (A.Function funId decs ex) = doDeclareFunction funId decs ex TyUnit
declareFunction (A.TypedFunction funId decs tyId ex) = do
  ty <- tyIdToType tyId
  doDeclareFunction funId decs ex ty

doDeclareFunction :: Identifier -> [A.FieldDeclaration] -> A.Expression -> Type -> Ty ()
doDeclareFunction funId decs ex ty = do
  tyId <- typeToTyId ty
  let argList = map (\(A.FieldDeclaration name argty) -> (name, argty)) decs
  let funtype = TyFunction tyId argList

  syms <- gets symbols 
  prolog <- gets prologs
  modify $ \s -> s { symbols = emptySymbols, prologs = [] }

  (expty, ex) <- checkExp ex
  stm <- unNx ex
  funpro <- gets prologs

  modify $ \s -> s { symbols = syms, functions = Map.insert funId (stm, funpro) (functions s), prologs = prolog }

  if expty == ty
    then addType funId funtype >> addVarWithType funId funId  -- creates a type named %1 and a symbol named %1
    else return $! error "unmatched expression type with function type"

-- perform typechecking on expression
checkExp :: A.Expression -> Ty (Type, Exp)

-- lvalue
checkExp (A.Value lval) = checkLVal lval

-- nil
checkExp A.Nil = do 
  ex <- nil
  return (TyNil, ex)

-- int literal
checkExp (A.IntLit i) = do 
  ex <- intLit i
  return (TyInt, ex)

-- string literal
checkExp (A.StringLit s) = do 
  ex <- strLit s
  return (TyStr, ex)

-- sequence expression
checkExp (A.SeqExp []) = do
  rtex <- seqexp []
  return $ (TyNil, rtex)
checkExp (A.SeqExp x) = do
  (tys, exps) <- fmap unzip $ mapM checkExp $ init x
  (rtty, rtexp) <- checkExp (last x)
  rtex <- seqexp $ exps ++ [rtexp]
  return $ (unwrapTypes tys $ rtty, rtex)

-- negation
checkExp (A.Negation e) = do
  (ty, ex) <- checkExp e
  rtex <- negation ex
  return $! if ty == TyInt then (ty, rtex) else error "only integer type allowed for negation"

-- call expression
checkExp (A.Call funcId exps) = do
  funcTy <- varIdToType funcId
  checkCallType funcTy exps funcId

-- infix operation
checkExp (A.Infix e1 op e2) = do
  (t1, ex1) <- checkExp e1
  (t2, ex2) <- checkExp e2
  rtex <- infixop ex1 op ex2
  return $! (checkInfixType t1 op t2, rtex)

-- array creation
checkExp (A.ArrCreate tyId num ival) = do
  arrty <- tyIdToType tyId
  (numty, numex) <- checkExp num
  (ivalty, ivalex) <- checkExp ival
  ty <- checkArrType arrty numty ivalty
  
  rtex <- array numex ivalex
  return $ (ty, rtex)

-- record creation
checkExp (A.RecCreate tyId fields) = do
  recty <- tyIdToType tyId
  checkRecType recty fields

-- assignment
checkExp (A.Assignment lval exp) = do
  (lvalty, lvalex) <- checkLVal lval
  (expty, expex) <- checkExp exp
  rtex <- assign lvalex expex
  return $! (checkType lvalty expty, rtex)

-- if then else
checkExp (A.IfThenElse e1 e2 e3) = do
  (condty, condex) <- checkExp e1
  (tty, tex) <- checkExp e2
  (fty, fex) <- checkExp e3
  rtex <- ifThenElse condex tex $ Just fex
  return $ (unwrapTypes [condty] $ checkIfThenElse condty tty fty, rtex)

-- if then
checkExp (A.IfThen e1 e2) = do
  (condty, condex) <- checkExp e1
  (tty, tex) <- checkExp e2
  rtex <- ifThenElse condex tex Nothing
  return $ (unwrapTypes [condty] $ checkIfThenElse condty tty TyUnit, rtex)

-- while
checkExp (A.While cond ex) = do
  blabel <- newLabel
  (ty, condex) <- checkExp cond
  (exty, exex) <- checkExp ex

  l <- getBreakLabel
  _ <- setBreakLabel blabel
  rtex <- while condex exex blabel
  _ <- setBreakLabel l

  return $! (unwrapTypes [exty] $ if ty == TyInt then TyUnit else error "invalid while condition type", rtex)

-- for to
checkExp (A.ForTo varId e1 e2 ex) = do
  (ty1, sex) <- checkExp e1
  (ty2, eex) <- checkExp e2
  
  blabel <- newLabel
  olabel <- getBreakLabel
  _ <- setBreakLabel blabel
  syms <- gets symbols
  _ <- addVarWithType varId "int"   -- adds the var declaration

  (exty, bodyex) <- checkExp ex
  rtex <- forto varId sex eex bodyex blabel
  
  modify $ \s -> s { symbols = syms }   -- .. and recover the scope
  _ <- setBreakLabel olabel

  return $! (unwrapTypes [exty] $ checkForTo ty1 ty2, rtex)

-- break
checkExp A.Break = do 
  rtex <- breakLoop
  return $ (TyUnit, rtex)

-- let in
checkExp (A.LetIn decs exps) = do
  syms <- gets symbols
  ty <- gets types 

  _ <- mapM_ checkDec decs
  (tys, exps) <- fmap unzip $ mapM checkExp exps

  modify $ \s -> s { symbols = syms, types = ty }

  stmts <- mapM unNx $ init exps
  rtex <- case last exps of
    Nx stm -> do return $ Nx $ Seq (seqstm stmts) stm
    _ -> do
      ex <- unEx $ last exps
      return $ Ex $ Eseq (seqstm stmts) ex
  
  return $! (unwrapTypes tys (last tys), rtex)

-- perform typecheck on declarations
checkDec :: A.Declaration -> Ty ()

-- type declaration
checkDec (A.TypeDec tyId ty) = declareType tyId ty
checkDec (A.VarDec var) = declareVar var
checkDec (A.FunDec fun) = declareFunction fun

-- IR definition

data Exp = 
  Ex Expr
  | Nx Stm
  | Cx (Label -> Label -> Stm)

data Expr = 
  Const Int
  | Name Label
  | Temp Temp
  | BinOp Expr BinOp Expr
  | Mem Expr
  | Call Expr [Expr]
  | Eseq Stm Expr
  deriving (Show)

data Stm = 
  Move Expr Expr
  | Expr Expr
  | Jump Expr [Label]
  | CJump Expr RelOp Expr Label Label
  | Seq Stm Stm
  | Label Label
  deriving (Show)

data BinOp = Plus | Minus | Mul | Div | And | Or | LShift | RShift | ARShift | Xor deriving (Show)
data RelOp = EQL | NE | LST | GRT | LE | GE | ULT | ULE | UGT | UGE deriving (Show)

seqstm :: [Stm] -> Stm
seqstm (x:xs) = Seq x $ seqstm xs
seqstm [x] = x
seqstm [] = error "empty statement list"

unEx :: Exp -> Ty Expr
unEx (Ex e) = return e
unEx (Nx s) = return $ Eseq s (Const 0)
unEx (Cx c) = do
  r <- newTemp
  t <- newLabel
  f <- newLabel
  return $ Eseq (seqstm [Move (Temp r) (Const 1), (c t f), Label f, Move (Temp r) (Const 0), Label t]) (Temp r)

unNx :: Exp -> Ty Stm
unNx (Nx s) = return s
unNx (Ex e) = return $ Expr e
unNx (Cx _) = do
  t <- newLabel
  return $ Label t

unCx :: Exp -> Ty (Label -> Label -> Stm)
unCx (Cx c) = return c
unCx (Ex (Const 0)) = return $ \t f -> Jump (Name f) [f]
unCx (Ex (Const 1)) = return $ \t f -> Jump (Name t) [t]
unCx (Ex e) = return $ \t f -> CJump e EQL (Const 0) f t
unCx (Nx _) = error "invalid conditional expression"

-- translator stuff

infixOpToRelOp x = 
  case x of
    A.Equal -> EQL
    A.NotEqual -> NE
    A.Greater -> GRT
    A.Less -> LST
    A.GreaterE -> GE
    A.LessE -> LE
    _ -> error "invalid infix for converting to relop"

infixOpToBinOp x =
  case x of
    A.Add -> Plus
    A.Sub -> Minus
    A.Multiply -> Mul
    A.Divide -> Div
    A.And -> And
    A.Or -> Or
    _ -> error "invalid infix for converting to binop"

infixType :: A.InfixOp -> Int
infixType c  
      | c `elem` [A.Add, A.Sub, A.Multiply, A.Divide, A.And, A.Or] = 0
      | c `elem` [A.Equal, A.NotEqual, A.Greater, A.Less, A.GreaterE, A.LessE] = 1
      | otherwise = error "invalid infix op"

ifThenElse :: Exp -> Exp -> Maybe Exp -> Ty Exp
ifThenElse cx (Ex tex) (Just (Ex fex)) = do
  c <- unCx cx
  r <- newTemp
  t <- newLabel
  f <- newLabel
  finish <- newLabel
  return $ Ex $ Eseq (seqstm [c t f, Label t, Move (Temp r) tex, Jump (Name finish) [finish], Label f, Move (Temp r) fex, Jump (Name finish) [finish], Label finish]) (Temp r)

ifThenElse cx (Nx tnx) Nothing = do
  c <- unCx cx
  t <- newLabel
  f <- newLabel
  return $ Nx $ seqstm [c t f, Label t, tnx, Label f]

ifThenElse cx (Nx tnx) (Just (Nx fnx)) = do
  c <- unCx cx
  t <- newLabel
  f <- newLabel
  finish <- newLabel
  return $ Nx $ seqstm [c t f, Label t, tnx, Jump (Name finish) [finish], Label f, fnx, Jump (Name finish) [finish], Label finish]

ifThenElse cx (Cx tcx) (Just (Cx fcx)) = do
  c <- unCx cx
  t <- newLabel
  f <- newLabel
  return $ Cx $ \t' f' -> seqstm [c t f, Label t, tcx t' f', Label f, fcx t' f']

ifThenElse _ _ _ = error "invalid if-then-else expression"
  
memoff :: Expr -> Expr -> Expr
memoff ex off = Mem (BinOp ex Plus (BinOp (Const 4) Mul off))

-- translate on expressions

intLit :: Int -> Ty Exp
intLit i = return $ Ex $ Const i

strLit :: String -> Ty Exp
strLit s = do
  l <- newLabel
  _ <- addString s l
  return $ Ex $ Name l

nil :: Ty Exp
nil = return $ Ex $ Const 0

negation :: Exp -> Ty Exp
negation e = do
  ex <- unEx e
  return $ Ex $ BinOp (Const 0) Minus ex

infixop :: Exp -> A.InfixOp -> Exp -> Ty Exp
infixop e1 op e2 = do
  ex1 <- unEx e1
  ex2 <- unEx e2
  return $
    case infixType op of 
      0 -> Ex $ BinOp ex1 (infixOpToBinOp op) ex2
      1 -> Cx $ \t f -> CJump ex1 (infixOpToRelOp op) ex2 t f

seqexp :: [Exp] -> Ty Exp
seqexp exps = do
  case (length exps) of
    0 -> return $ Ex $ Const 0
    1 -> return $ exps !! 0
    _ -> do
      ex <- mapM unNx $ init exps
      case last exps of
        Nx stm -> return $ Nx $ Seq (seqstm ex) stm
        _ -> do
          rt <- unEx $ last exps
          return $ Ex $ Eseq (seqstm ex) rt

breakLoop :: Ty Exp
breakLoop = do
  l <- getBreakLabel
  return $ Nx $ Jump (Name l) [l]

while :: Exp -> Exp -> Label -> Ty Exp
while cond body donel = do
  t <- newLabel
  bodyl <- newLabel
  ex <- unEx cond
  stm <- unNx body
  return $ Nx $ seqstm [Label t, CJump ex EQL (Const 0) donel bodyl, Label bodyl, stm, Jump (Name t) [t], Label donel]

forto :: String -> Exp -> Exp -> Exp -> Label -> Ty Exp
forto varid s e body donel = do
  ref <- getVarRef varid
  let var = ref
  
  bodyl <- newLabel
  t <- newLabel
  f <- newLabel
  sv <- unEx s
  ev <- unEx e
  bd <- unNx body

  return $ Nx $ seqstm [Move var sv, Label bodyl, CJump var GE ev donel bodyl, Label bodyl, Move var (BinOp var Plus (Const 1)), bd, Jump (Name bodyl) [bodyl], Label donel]

assign :: Exp -> Exp -> Ty Exp
assign a b = do 
  exa <- unEx a
  exb <- unEx b

  return $ Nx $ Move exa exb

-- create array and record with the help of runtime
array :: Exp -> Exp -> Ty Exp
array numex ivalex = do
  num <- unEx numex
  ival <- unEx ivalex
  return $ Ex $ Call (Name "createArray") [num, ival]

record :: [Exp] -> Ty Exp
record exps = do
  let size = M.wordSize * (length exps)
  r <- newTemp
  exs <- fmap (zip [0..]) $ mapM unEx exps
  let stmts = map (\(idx, ex) -> Move (memoff (Temp r) (Const idx)) ex) exs
  return $ Ex $ Eseq (seqstm ([Move (Temp r) (Call (Name "createRecord") $ [(Const size)])] ++ stmts)) (Temp r)

call :: Label -> [Exp] -> Ty Exp
call l exs = do
  exps <- mapM unEx exs
  return $ Ex $ Call (Name l) exps

offset :: Exp -> Exp -> Ty Exp
offset base off = do
  b <- unEx base
  o <- unEx off
  return $ Ex $ Mem (BinOp b Plus (BinOp (Const M.wordSize) Mul o))


