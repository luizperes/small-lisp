module Interpreter where

import qualified Data.Map.Strict as Map
import System.Environment
import Data.Char (isDigit)
import Parser
import Unparser

-- 
-- lookup in map is O(logn)
--
type Map = Map.Map
type FunEnv = (Identifier, ([Identifier], SmLispExpr))
type FunEnvMap = Map Identifier ([Identifier], SmLispExpr)
type VarEnv = (Identifier, SExpression)
type VarEnvMap = Map Identifier SExpression

globalFrame = Map.fromList
            [("otherwise", SymAtom "T"),
             ("T", SymAtom "T"),
             ("F", SymAtom "F"),
             ("bottom", SymAtom "Error")]

interpret :: [Definition] -> SmLispExpr -> [Char]
interpret defs expr =
  case setupEnvs defs ([], []) of
    (fnEnv, varEnv) -> unparse (eval expr fnEnv (Map.union varEnv globalFrame))

setupEnvs :: [Definition] -> ([FunEnv], [VarEnv]) -> (FunEnvMap, VarEnvMap)
setupEnvs [] (fnEnv, varEnv) = (Map.fromList fnEnv, Map.fromList varEnv)
setupEnvs (x:xs) (fnEnv, varEnv) =
  case x of
    ConstantDef _ id expr ->
      setupEnvs xs (fnEnv, varEnv ++ [(id, eval expr (Map.fromList fnEnv) (Map.fromList varEnv))])
    FunctionDef _ id params expr ->
      setupEnvs xs (fnEnv ++ [(id, (params, expr))], varEnv)

eval :: SmLispExpr -> FunEnvMap -> VarEnvMap -> SExpression
eval (SExpr expr) _ _ = expr
eval (Variable v) _ varEnv =
  case Map.lookup v varEnv of
    Just value -> value
    _ -> SymAtom "Error"
eval (FnCall id values) fnEnv varEnv =
  case (map (\x -> eval x fnEnv varEnv) values) of
    args -> case (apply id args fnEnv varEnv) of
      Just se -> se
      _ -> SymAtom "Error"
eval (CondExpr conds) fnEnv varEnv =
  case (applyCond conds fnEnv varEnv) of
    Just se -> se
    _ -> SymAtom "Error"
eval (LetExpr defs expr) fnEnv varEnv =
  case (applyLet defs fnEnv varEnv) of
    (newVarEnv, True) -> eval expr fnEnv newVarEnv
    _ -> SymAtom "Error"
eval (MapExpr id exprs) fnEnv varEnv =
  case (map (\x -> eval x fnEnv varEnv) exprs) of
    args -> applyMap id args fnEnv varEnv
eval (ReduceExpr id expr) fnEnv varEnv =
  case (eval expr fnEnv varEnv) of
    (List []) -> SymAtom "Error"
    (List lst) -> applyReduce id lst fnEnv varEnv
    _ -> SymAtom "Error"

apply :: Identifier -> [SExpression] -> FunEnvMap -> VarEnvMap -> Maybe SExpression
apply id args fnEnv varEnv
  | id == "symbolp"   = symbolp (head args) varEnv
  | id == "numberp"   = numberp (head args) varEnv
  | id == "listp"     = listp (head args) varEnv
  | id == "endp"      = endp (head args) varEnv
  | id == "first"     = first (head args) varEnv
  | id == "rest"      = rest (head args) varEnv
  | id == "cons"      = cons (head args) (head(tail args)) varEnv
  | id == "eq"        = eq (head args) (head(tail args)) varEnv
  | id == "plus"      = plus (head args) (head(tail args)) varEnv
  | id == "minus"     = minus (head args) (head(tail args)) varEnv
  | id == "times"     = times (head args) (head(tail args)) varEnv
  | id == "divide"    = divide (head args) (head(tail args)) varEnv
  | id == "rem"       = drem (head args) (head(tail args)) varEnv
  | id == "eqp"       = eqp (head args) (head(tail args)) varEnv
  | id == "lessp"     = lessp (head args) (head(tail args)) varEnv
  | id == "greaterp"  = greaterp (head args) (head(tail args)) varEnv
  | id == "sym-lessp" = symlessp (head args) (head(tail args)) varEnv
  | id == "explode"   = explode (head args) [] varEnv
  | id == "implode"   = implode (head args) "" varEnv
  | id == "error"     = derror (head args)
  | otherwise = case (Map.lookup id fnEnv) of
    Just (bindings, expr) ->
      Just (eval
        expr
        fnEnv
        (Map.union (Map.fromList (zipWith (\x y -> (x, y)) bindings args)) varEnv))
    _ -> Nothing

applyCond :: [CondClause] -> FunEnvMap -> VarEnvMap -> Maybe SExpression
applyCond [] _ varEnv = Nothing
applyCond ((cond, expr):conds) fnEnv varEnv =
  case (eval cond fnEnv varEnv) of
    (SymAtom "T") -> Just (eval expr fnEnv varEnv)
    (SymAtom "F") -> applyCond conds fnEnv varEnv
    _ -> Nothing

applyLet :: [LocalDef] -> FunEnvMap -> VarEnvMap -> (VarEnvMap, Bool)
applyLet [] _ varEnv = (varEnv, True)
applyLet ((id, expr):defs) fnEnv varEnv =
  case (eval expr fnEnv varEnv) of
    (SymAtom "Error") -> (varEnv, False)
    evalExpr -> applyLet defs fnEnv (Map.union (Map.fromList [(id, evalExpr)]) varEnv)

applyReduce :: Identifier -> [SExpression] -> FunEnvMap -> VarEnvMap -> SExpression
applyReduce id lst fnEnv varEnv =
  foldl1 (\acc expr ->
    case (apply id (acc:expr:[]) fnEnv varEnv) of
      Just newExpr -> newExpr
      _ -> SymAtom "Error") lst

applyMap :: Identifier -> [SExpression] -> FunEnvMap -> VarEnvMap -> SExpression
applyMap id lst fnEnv varEnv =
  foldl1 (\acc expr ->
    case acc of
      (List nlst) -> case expr of
        (List nlst2) -> case (length nlst) == (length nlst2) of
          True -> List (zipWith (\nAcc nExpr -> case (apply id (nAcc:nExpr:[]) fnEnv varEnv) of
            Just newExpr ->  newExpr
            _ -> SymAtom "Error") nlst nlst2)
          _ -> SymAtom "Error"
        _ -> SymAtom "Error"
      _ -> SymAtom "Error") lst
symbolp :: SExpression -> VarEnvMap -> Maybe SExpression
symbolp (SymAtom _) varEnv = Map.lookup "T" varEnv
symbolp _ varEnv = Map.lookup "F" varEnv

numberp :: SExpression -> VarEnvMap -> Maybe SExpression
numberp (NumAtom _) varEnv = Map.lookup "T" varEnv
numberp _ varEnv = Map.lookup "F" varEnv

listp :: SExpression -> VarEnvMap -> Maybe SExpression
listp (List _) varEnv = Map.lookup "T" varEnv
listp _ varEnv = Map.lookup "F" varEnv

endp :: SExpression -> VarEnvMap -> Maybe SExpression
endp (List []) varEnv = Map.lookup "T" varEnv
endp (List _) varEnv = Map.lookup "F" varEnv
endp _ varEnv = Map.lookup "bottom" varEnv

first :: SExpression -> VarEnvMap -> Maybe SExpression
first (List []) varEnv = Map.lookup "bottom" varEnv
first (List (x:xs)) varEnv = Just x
first _ varEnv = Map.lookup "bottom" varEnv

rest :: SExpression -> VarEnvMap -> Maybe SExpression
rest (List []) varEnv = Map.lookup "bottom" varEnv
rest (List (x:xs)) varEnv = Just (List xs)
rest _ varEnv = Map.lookup "bottom" varEnv

cons :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
cons x (List xs) _ = Just (List (x:xs))
cons _ _ varEnv = Map.lookup "bottom" varEnv

eq :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
eq (SymAtom x) (SymAtom y) varEnv
  | x == y = Map.lookup "T" varEnv
  | otherwise = Map.lookup "F" varEnv
eq _ _ varEnv = Map.lookup "bottom" varEnv

plus :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
plus (NumAtom x) (NumAtom y) varEnv = Just (NumAtom (x+y))
plus _ _ varEnv = Map.lookup "bottom" varEnv

minus :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
minus (NumAtom x) (NumAtom y) varEnv = Just (NumAtom (x-y))
minus _ _ varEnv = Map.lookup "bottom" varEnv

times :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
times (NumAtom x) (NumAtom y) varEnv = Just (NumAtom (x*y))
times _ _ varEnv = Map.lookup "bottom" varEnv

divide :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
divide (NumAtom x) (NumAtom y) varEnv
  | y /= 0 = Just (NumAtom (x `div` y))
  | otherwise = Map.lookup "bottom" varEnv
divide _ _ varEnv = Map.lookup "bottom" varEnv

drem :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
drem (NumAtom x) (NumAtom y) varEnv
  | y /= 0 = Just (NumAtom (x `Prelude.rem` y))
  | otherwise = Map.lookup "bottom" varEnv
drem _ _ varEnv = Map.lookup "bottom" varEnv

eqp :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
eqp (NumAtom x) (NumAtom y) varEnv
  | x == y = Map.lookup "T" varEnv
  | otherwise = Map.lookup "F" varEnv
eqp _ _ varEnv = Map.lookup "bottom" varEnv

lessp :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
lessp (NumAtom x) (NumAtom y) varEnv
  | x <= y = Map.lookup "T" varEnv
  | otherwise = Map.lookup "F" varEnv
lessp _ _ varEnv = Map.lookup "bottom" varEnv

greaterp :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
greaterp (NumAtom x) (NumAtom y) varEnv
  | x >= y = Map.lookup "T" varEnv
  | otherwise = Map.lookup "F" varEnv
greaterp _ _ varEnv = Map.lookup "bottom" varEnv

symlessp :: SExpression -> SExpression -> VarEnvMap -> Maybe SExpression
symlessp (SymAtom x) (SymAtom y) varEnv
  | x < y = Map.lookup "T" varEnv
  | otherwise = Map.lookup "F" varEnv
symlessp _ _ varEnv = Map.lookup "bottom" varEnv

explode :: SExpression -> [SExpression] -> VarEnvMap -> Maybe SExpression
explode (SymAtom []) acc _ = Just (List acc)
explode (SymAtom (x:xs)) acc varEnv
  | isDigit x = explode (SymAtom xs) (acc ++ [NumAtom (read [x]::Int)]) varEnv
  | otherwise = explode (SymAtom xs) (acc ++ [SymAtom [x]]) varEnv
explode _ _ varEnv = Map.lookup "bottom" varEnv

implode :: SExpression -> [Char] -> VarEnvMap -> Maybe SExpression
implode (List []) acc _ = Just (SymAtom acc)
implode (List (l:ls)) acc varEnv =
  case onlyAtoms (l:ls) "" True of
    (syms, True) -> Just (SymAtom syms)
    _ -> Map.lookup "bottom" varEnv
implode _ _ varEnv = Map.lookup "bottom" varEnv

onlyAtoms :: [SExpression] -> [Char] -> Bool -> ([Char], Bool)
onlyAtoms [] accS accB = (accS, accB)
onlyAtoms (e:es) accS accB =
  case e of
    (NumAtom n) -> onlyAtoms es (accS ++ (show n)) accB -- Implicit acc && True
    (SymAtom s) -> onlyAtoms es (accS ++ s) accB
    _ -> onlyAtoms es accS (accB && False) -- accS will be ignored
    
derror :: SExpression -> Maybe SExpression
derror x = Just (SymAtom ("Error: " ++ (unparse x)))
