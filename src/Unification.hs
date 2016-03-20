{-# LANGUAGE OverloadedStrings #-}

module Unification where

import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.State  (StateT, evalStateT, get, modify)
import           Control.Monad.Supply (Supply, evalSupply, supply)
import qualified Data.Map             as M
import           Data.String          (IsString, fromString)

type Id = String

data Term = Const Id
          | Var Id
          | App Id [Term]
          deriving (Eq, Show)

data LTerm = LVar Id
           | LApp LTerm LTerm
           | LFun Id LTerm
           | LInt Int
           deriving (Show, Eq)

data Type = TVar Id
          | TArr Type Type
          | TInt
          deriving (Eq)

instance Show Type where
  show (TVar x) = x
  show TInt = "Int"
  show (TArr t t') = "(" ++ show t ++ " -> " ++ show t' ++ ")"

data ATerm = AVar Id Type
           | AApp ATerm ATerm Type
           | AFun Id ATerm Type
           | AInt Int
           deriving (Eq, Show)

termToType :: Term -> Type
termToType (Const "Int") = TInt
termToType (Var x) = TVar x
termToType (App _ [l, r]) = TArr (termToType l) (termToType r)

typeToTerm :: Type -> Term
typeToTerm TInt = Const "Int"
typeToTerm (TVar x) = Var x
typeToTerm (TArr l r) = App "->" [typeToTerm l, typeToTerm r]

(@..) :: Id -> [Term] -> Term
(@..) = App

(@.) :: Id -> Term -> Term
(@.) i t = App i [t]

type Subst = [(Id, Term)]
type Equation = (Term, Term)
type EqSet = [Equation]

subst :: (Id, Term) -> Term -> Term
subst _        c@(Const _) = c
subst (x, t)   v@(Var x')  = if x == x' then t else v
subst s@(x, t) (App f ts)  = App f (map (subst s) ts)

apply :: Term -> Subst -> Term
apply = foldr subst

occurs :: Id -> Term -> Bool
occurs _ (Const _)  = False
occurs x (Var x')   = x == x'
occurs x (App _ xs) = any (occurs x) xs

unifyEq :: Equation -> Either String Subst
unifyEq e@(App _ _, Const _) = throwError ("Can't match constant with app in " ++ show e)
unifyEq e@(Const _, App _ _) = throwError ("Can't match constant with app in " ++ show e)
unifyEq (App f ts, App f' ts')
    | f /= f'                 = throwError "Symbol clash"
    | length ts /= length ts' = throwError "Failed decomposition"
    | otherwise               = unify (zip ts ts') -- decomposition (f(x1,..., xn), f(y1,...,yn)) => {(x1,y1),...,(xn, yn)}
unifyEq (Var x, t@(Var y))
    | x == y    = return []        -- trivial (x, x) => {}
    | otherwise = return [(x, t)]  -- var elim (x, t) => {(x, t)}
unifyEq (Var x, t)
    | occurs x t = throwError "Occur check"
    | otherwise  = return [(x, t)]  -- var elim (x, t) => {(x, t)}
unifyEq (t, Var x)
    | occurs x t = throwError "Occur check"
    | otherwise  = return [(x, t)]  -- var elim (x, t) => {(x, t)}

unify :: EqSet -> Either String Subst
unify [] = return []
unify ((t, t'):ts) = do
    s1 <- unify ts
    s2 <- unifyEq (apply t s1, apply t' s1)
    return (s1 ++ s2)

lup :: (Ord k) => k -> M.Map k v -> Either String v
lup k v = maybe (throwError "Not found") return (M.lookup k v)

typeOf :: ATerm -> Type
typeOf (AInt _) = TInt
typeOf (AVar _ t) = t
typeOf (AApp _ _ t) = t
typeOf (AFun _ _ t) = t

annotate :: LTerm -> ATerm
annotate t = evalSupply (evalStateT (annotate' M.empty t) M.empty) tvars
  where
    tvars :: [Type]
    tvars = TVar . ("a" ++) . show <$> [0..]

    lupFV :: Id -> StateT (M.Map Id Type) (Supply Type) Type
    lupFV x = do
      fv <- get
      case M.lookup x fv of
        Just t -> return t
        _      -> do
          a <- supply
          modify (M.insert x a)
          return a

    annotate' :: M.Map Id Type -> LTerm -> StateT (M.Map Id Type) (Supply Type) ATerm
    annotate' bv (LInt x) = return (AInt x)
    annotate' bv (LVar x) = do
      a <- case M.lookup x bv of
          Just t -> return t
          _      -> lupFV x
      return (AVar x a)
    annotate' bv (LApp f x) = do
      af <- annotate' bv f
      ax <- annotate' bv x
      a  <- supply
      return (AApp af ax a)
    annotate' bv (LFun i t) = do
      a  <- supply
      ae <- annotate' (M.insert i a bv) t
      return (AFun i ae (TArr a (typeOf ae)))

collect :: ATerm -> EqSet
collect t = collect' [t] []
  where
    collect' [] es = es
    collect' (t:ts) es = case t of
      (AInt _)       -> collect' ts es
      (AVar _ _)     -> collect' ts es
      (AFun _ ae _)  -> collect' (ae:ts) es
      (AApp af ax a) -> let (f, b) = (typeOf af, typeOf ax)
                        in collect' (af:ax:ts) ((typeToTerm f, typeToTerm (TArr b a)):es)

infer :: LTerm -> Either String Type
infer t = let annotation   = annotate t
              equations    = collect annotation
              substitution = unify equations
              unified      = apply (typeToTerm $ typeOf annotation) <$> substitution
          in termToType <$> unified

main = do
  putStrLn . either show show . infer $ LApp (LApp (LFun "x" (LFun "y" (LApp (LVar "x") (LVar "y")))) (LFun "y" (LVar "y"))) (LFun "x" (LVar "x"))
  putStrLn . either show show . infer $ LApp (LFun "x" (LApp (LVar "x") (LInt 3))) (LFun "x" (LInt 4))
  putStrLn . either show show . infer $ LFun "x" (LFun "y" (LVar "x"))
  putStrLn . either show show . infer $ LFun "x" (LFun "y" (LApp (LVar "x") (LApp (LVar "x") (LVar "y"))))
