{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module CTProver where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Tree
import Control.Lens
import Control.Monad.State
import TypeDef
import Unifier

type Namer = Map Text Int

stripDigits :: Text → Text
stripDigits = T.dropWhileEnd (isDigit)

newName :: MonadState s m => Lens' s Namer → Text → m Text
newName l templ' =
  let templ = stripDigits templ' in
  l . at templ %%= \case
    Nothing -> (templ, Just 0)
    Just n -> (T.append templ (T.pack (show n)), Just (n + 1))

replaceVars :: Eq a => [(a,a)] → Rule a → Rule a
replaceVars assoc (Rule concl prems) =
  Rule (replaceVarsRec assoc concl) (replaceVarsRec assoc <$> prems)
  where
    replaceVarsRec assoc (Var x) = case (lookup x assoc) of
      Nothing → error "fail in replaceVarsRec"
      Just x' → Var x'
    replaceVarsRec assoc (Atom a recs) =
      Atom a $ replaceVarsRec assoc <$> recs

listVarsRule :: Rule a → [a]
listVarsRule (Rule concl prems) =
  listVars concl ++ concat (fmap listVars prems)

freshVars :: MonadState s m => Lens' s Namer → Rule Text → m (Rule Text)
freshVars l rule = do
  assoc ← traverse (\x → fmap (x,) $ newName l x) (listVarsRule rule)
  return $ replaceVars assoc rule

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

wireTree :: (Ord a) => Tree (Rule a) →
            Either (UnifierError (Maybe a)) (Tree (Rec a))
wireTree (Node (Rule concl premises) subtrees) =
  do
    subtrees ← traverse wireTree subtrees
    store ← unifyLists premises $ rootLabel <$> subtrees
    return $ eval store <$> (Node concl subtrees)

decipherTree :: Map RuleName (Rule Text) →
              Tree RuleName →
              Either (UnifierError (Maybe Text)) (Tree (Rec Text))
decipherTree theory tree =
  let
    ruleTree' = tree <&> \x → case M.lookup x theory of
      Nothing → error "fail in unpackTree"
      Just x → x
    ruleTree = evalState (traverse (freshVars id) ruleTree') M.empty
  in wireTree ruleTree
