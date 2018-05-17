{-# LANGUAGE UnicodeSyntax #-}

module Unifier (eval, unify) where

import TypeDef
import qualified Data.Map as M
import Data.Monoid (First(..))
import Data.List
import Control.Monad.Except
import Control.Monad.State

-- unite equivalency classes m n in store
uniteEqClasses :: Int → Int → Store a → Store a
uniteEqClasses m n st = fmap (uniteEqClasses' m n) st where
  uniteEqClasses' m n (EqClass x) =
    if x == max m n
    then EqClass (min m n)
    else EqClass x
  uniteEqClasses' _ _ x = x

-- bind all variables refering to class m to variable v
bindVal :: Int → Rec a → Store a → Store a
bindVal m v st = fmap (bindVal' m v) st where
  bindVal' m v (EqClass x) =
    if x == m
    then Val v
    else EqClass x
  bindVal' _ _ x = x

type UnifMonad a = StateT (Store a, [(a, a)]) (Either (UnifierError a))

modifyStore :: (Store a → Store a) → UnifMonad a ()
modifyStore f =
  modify (\(st,memo) → (f st, memo))

-- check if pair x is already unified
checkMemo :: Eq a ⇒ (a,a) → UnifMonad a Bool
checkMemo x =
  fmap (elem x . snd) get

-- check record pair x as already unified
putMemo :: (a,a) → UnifMonad a ()
putMemo x =
  modify (\(st,memo) → (st, x : memo))

-- unite classes n and m in the monad
uniteEqClassesM :: Int → Int → UnifMonad a ()
uniteEqClassesM n m = modifyStore (uniteEqClasses n m)

-- bind class to variable in the monad
bindValM :: Int → Rec a → UnifMonad a ()
bindValM n v = modifyStore (bindVal n v)

-- lookup variable x in Store
lookUpStore :: (Show a, Ord a) ⇒ a → UnifMonad a (MemCell a)
lookUpStore x = do
  st ← fmap fst get
  case M.lookup x st of
    Nothing → throwError $ NotInitialized x
    Just v → return v

-- unify records
unifyStepR :: (Show a, Ord a) ⇒ Rec a → Rec a → UnifMonad a ()
-- two variables are unified by unifying memory cells
unifyStepR (Var a0) (Var a1) =
  do
    mc0 ← lookUpStore a0
    mc1 ← lookUpStore a1
    unifyStepMC mc0 mc1
-- one variable is unified with value by unifying its memory cell with
-- the coerced value
unifyStepR (Var a) v =
  do
    mc0 ← lookUpStore a
    unifyStepMC mc0 (Val v)
unifyStepR v (Var a) =
  do
    mc0 ← lookUpStore a
    unifyStepMC mc0 (Val v)
-- records are unified by checking their compability and unifying arguments
unifyStepR a1@(Atom i vs1) a2@(Atom j vs2) =
  if i /= j 
  then throwError $ IncompabitibleAtoms i j
  else if length vs1 /= length vs2
       then throwError $ ConflictingArity i
       else sequence_ $ zipWith unifyStepR vs1 vs2

-- unify memcells
unifyStepMC :: (Show a, Ord a) ⇒ MemCell a → MemCell a → UnifMonad a ()
-- classes are unified by union
unifyStepMC (EqClass i) (EqClass j) =
  uniteEqClassesM i j
-- class and value are unified by binding
unifyStepMC (EqClass i) (Val v) =
  bindValM i v
unifyStepMC (Val v) (EqClass i) =
  bindValM i v
-- two values are unified as records
unifyStepMC (Val v1) (Val v2) =
  unifyStepR v1 v2

-- lists variables of a record
listVars :: Rec a → [a]
listVars (Var a) = [a]
listVars (Atom i rs) = rs >>= listVars

-- lists variables of a memcell
listVarsMC :: MemCell a → [a]
listVarsMC (Val v) = listVars v
listVarsMC (EqClass _) = []

-- detects cycles in store
findCycle :: Ord a ⇒ Store a → Maybe a
findCycle store = getFirst $ foldMap (First . dig []) vars where
  vars = M.keys store -- list all variable
  -- lookup of dependencies for a variable
  deps var = case M.lookup var (fmap listVarsMC store) of
    Nothing → []
    Just vs → vs
  -- dig build a list of encountered variables in rope and
  -- checks every dependency of the variable, returning first found
  -- duplicate
  dig rope var =
    if var `elem` rope
    then Just var
    else getFirst $ foldMap (First . dig (var : rope)) $ deps var

-- evaluate a variable using a store without cycles
eval :: Ord a ⇒ Store a → Rec a → Rec a
eval store (Var var) =
  case M.lookup var store of
    Nothing → Var var
    Just (EqClass _) → Var var
    Just (Val v) → eval store v
eval store (Atom a vs) = Atom a $ fmap (eval store) vs

unify :: (Show a, Ord a) ⇒ Rec a → Rec a → Either (UnifierError a) (Store a)
unify x y =
  let
    vars = listVars x `union` listVars y
    initStore = M.fromList $ zip vars (fmap EqClass [0..])
    eitherStore = fmap fst $ execStateT (unifyStepR x y) (initStore,[])
  in
    do
      store ← eitherStore
      case findCycle store of
        Nothing → return store
        Just var → throwError $ CyclicVar var
  
