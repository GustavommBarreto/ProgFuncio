module Interpreter where

import AbsLF
import Prelude hiding (lookup)

-- ========================
-- Funções auxiliares
-- ========================

getName :: Function -> Ident
getName (Fun _ name _ _) = name

getParams :: Function -> [Ident]
getParams (Fun _ _ decls _) = [ ident | Dec _ ident <- decls ]

getExp :: Function -> Exp
getExp (Fun _ _ _ exp) = exp

-- ========================
-- Execução principal
-- ========================

executeP :: Program -> Valor
executeP (Prog fs) = eval (updatecF [] fs) (expMain fs)
  where
    expMain (f:xs)
      | (getName f == Ident "main") = getExp f
      | otherwise = expMain xs

-- ========================
-- Avaliador de expressões
-- ========================

eval :: RContext -> Exp -> Valor
eval context x = case x of
    ECon e1 e2   -> ValorStr (s (eval context e1) ++ s (eval context e2))
    EAdd e1 e2   -> ValorInt (i (eval context e1) + i (eval context e2))
    ESub e1 e2   -> ValorInt (i (eval context e1) - i (eval context e2))
    EMul e1 e2   -> ValorInt (i (eval context e1) * i (eval context e2))
    EDiv e1 e2   -> ValorInt (i (eval context e1) `div` i (eval context e2))
    EOr e1 e2    -> ValorBool (b (eval context e1) || b (eval context e2))
    EAnd e1 e2   -> ValorBool (b (eval context e1) && b (eval context e2))
    ENot e       -> ValorBool (not (b (eval context e)))
    EStr str     -> ValorStr str
    ETrue        -> ValorBool True
    EFalse       -> ValorBool False
    EInt n       -> ValorInt n
    EVar id      -> lookup context id

    -- if (cond) then expT else expE
    -- cond precisa ser inteiro ≠ 0
    EIf cond expT expE ->
        if i (eval context cond) /= 0
           then eval context expT
           else eval context expE

    -- chamada de função
    ECall id args ->
        eval (paramBindings ++ contextFunctions) (getExp funDef)
      where
        (ValorFun funDef) = lookup context id
        parameters        = getParams funDef
        paramBindings     = zip parameters (map (eval context) args)
        contextFunctions  = filter (\(_,v) -> case v of
                                                  ValorFun _ -> True
                                                  _          -> False) context

-- ========================
-- Definição de valores
-- ========================

data Valor
    = ValorInt  { i :: Integer }
    | ValorFun  { f :: Function }
    | ValorStr  { s :: String }
    | ValorBool { b :: Bool }

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i)  = show i
  show (ValorStr s)  = s
  show (ValorFun f)  = show f

type RContext = [(Ident, Valor)]

-- ========================
-- Manipulação de contexto
-- ========================

lookup :: RContext -> Ident -> Valor
lookup ((i,v):cs) s
   | i == s    = v
   | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s    = (i,nv):cs
  | otherwise = (i,v) : update cs s nv

updatecF :: RContext -> [Function] -> RContext
updatecF c []     = c
updatecF c (f:fs) = updatecF (update c (getName f) (ValorFun f)) fs
