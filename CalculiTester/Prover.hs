{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module CalculiTester.Prover (
  newName,
  freshVars,
  decipherTree,
  zipTree,
  Namer) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Tree
import Data.List (nub)
import Control.Lens
import Control.Monad.State
import CalculiTester.TypeDef
import CalculiTester.Unifier

--remembers the used names
type Namer = Map Text Int

--remove digits in the end
stripDigits :: Text → Text
stripDigits = T.dropWhileEnd (isDigit)

--generate a new name based on a template
newName :: MonadState s m => Lens' s Namer → Text → m Text
newName l templ' =
  --the template shouldn't have digits in the end
  let templ = stripDigits templ' in
    --check how many times the template has been used
    l . at templ %%= \case
    --if it hadn't, record it and return the bare template
    Nothing -> (templ, Just 0)
    --otherwise append the record to the template and
    --increment it
    Just n -> (T.append templ (T.pack (show n)), Just (n + 1))

--replace variable names in the rule according to the assoc
--expects replacements for all variables
replaceVars :: Eq a => [(a,a)] → Rule a → Rule a
replaceVars assoc (Rule concl prems) =
  Rule (replaceVarsRec assoc concl) (replaceVarsRec assoc <$> prems)
  where
    replaceVarsRec assoc (Var x) = case (lookup x assoc) of
      Nothing → error "fail in replaceVarsRec"
      Just x' → Var x'
    replaceVarsRec assoc (Atom a recs) =
      Atom a $ replaceVarsRec assoc <$> recs

--lists the variable name used in the rule
listVarsRule :: Eq a => Rule a → [a]
listVarsRule (Rule concl prems) =
  nub $ listVars concl ++ concat (fmap listVars prems)

--traverse the subtrees first
traverseUp :: (Applicative f) => (a → f b) → Tree a → f (Tree b)
traverseUp f (Node a trees) =
  flip Node <$> traverse (traverseUp f) trees <*> f a

--make all variables in the rule fresh
freshVars :: MonadState s m => Lens' s Namer → Rule Text → m (Rule Text)
freshVars l rule = do
  assoc ← traverse (\x → fmap (x,) $ newName l x) (listVarsRule rule)
  return $ replaceVars assoc rule

--unify corresponding expression with awareness about other
--simple traversal would make incoherent unifications
unifyLists :: (Ord a) =>
              [Rec a] → [Rec a] → Either (UnifierError (Maybe a)) (Store a)
unifyLists recs recs' =
  let
    fromJust :: Maybe a → a
    fromJust = \case
      Nothing → error "fail in unifyLists"
      Just x → x
    collect = Atom Nothing . fmap (fmap Just)
  in unify (collect recs) (collect recs') &
     over _Right (M.mapKeysMonotonic fromJust . fmap (fmap fromJust))

--like zip, but for trees
zipTree :: Tree a → Tree b → Tree (a, b)
zipTree (Node a as) (Node b bs) =
  Node (a, b) $ zipWith zipTree as bs

--unifies premises of the head rule with conclusions of the subtrees
wireTree :: (Ord a) => Tree (Rule a) →
            Either (UnifierError (Maybe a)) (Tree (Rec a))
wireTree (Node (Rule concl premises) subtrees) =
  do
    subtrees ← traverse wireTree subtrees
    store ← unifyLists premises (rootLabel <$> subtrees) 
    return $ eval store <$> (Node concl subtrees)

--transforms text proof into expression proof
--with error reporting
decipherTreeOld :: Theory Text →
              Tree RuleName →
              Either (UnifierError (Maybe Text)) (Tree (Rec Text))
decipherTreeOld theory tree =
  let
    --replace names of rule with their expressions
    ruleTree' = tree <&> \x → case M.lookup x theory of
      Nothing → error "fail in decipherTree"
      Just x → x
    --put fresh variables into each rule
    ruleTree = evalState (traverseUp (freshVars id) ruleTree') M.empty
  in wireTree ruleTree --connect the rule together

--transforms text proof into expression proof
--without error reporting
decipherTree :: Theory Text → Tree RuleName → Maybe (Tree (Rec Text))
decipherTree theory tree = do
  ruleTree' ← traverse (\x → M.lookup x theory) tree
  let ruleTree = evalState (traverseUp (freshVars id) ruleTree') M.empty
  wireTree ruleTree & \case
    Left _ → Nothing
    Right x → Just x
