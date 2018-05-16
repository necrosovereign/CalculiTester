{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module CTFileReader (loadTheory) where

import TypeDef
import CTParser
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.RWS
import Control.Monad.Except
import Control.Lens

data LoadState = LoadState
  {
    _lineNumber :: Int,
    _inParagraph :: Bool,
    _fixity :: Fixity,
    _heading :: Either OperType RuleName,
    _currentRuleContext :: [Rec Text]
  }

makeLenses ''LoadState

type LoadMonad = RWST [Text] (Map RuleName (Rule Text)) LoadState
  (Either FileReaderError)

loadTheory :: FilePath → IO (Either FileReaderError (Theory Text))
loadTheory filename = 
  fmap (loadTheoryList . T.lines) $ T.readFile filename

loadTheoryList :: [Text] → Either FileReaderError (Theory Text)
loadTheoryList text = _Right . _1 %~ (view fixity) $
  execRWST consumeLine text initLoadState 

initLoadState :: LoadState
initLoadState =
  LoadState {
  _lineNumber = 1,
    _inParagraph = False,
    _fixity = M.fromList [("#", Infix ARight 0)],
    _heading = Right "",
    _currentRuleContext = []
  }

consumeLine :: LoadMonad ()
consumeLine =
  asks null >>=
  \case
    True → finishParagraph
    False →
      do
        b ← use inParagraph
        if b
          then consumeText
          else consumeHeading
        lineNumber %= (+1)
        local tail consumeLine

consumeHeading :: LoadMonad ()
consumeHeading = do
  line ← asks head
  let (start, rulename) = T.splitAt 2 line
  when (start `elem` ["--", "in", "pr", "po"]) $
    inParagraph .= True
  heading .= (
    case start of
      "--" → Right $ rulename
      "in" → Left $ Infix ALeft 1
      "pr" → Left $ Prefix
      "po" → Left $ Postfix
      _ → Right ""
    )

consumeText :: LoadMonad ()
consumeText = do
  paragraphType ← use heading
  case paragraphType of
    Left _ → consumeOper
    Right _ → consumeRule

doOrFinish :: (Text → LoadMonad ()) → LoadMonad ()
doOrFinish consumer = do
  line ← fmap T.strip $ asks head
  if T.null line
    then finishParagraph
    else consumer line

consumeOper :: LoadMonad ()
consumeOper =
  let
    reassoc (Infix _ n) = Infix ARight n
    reassoc x = x
    raisePrec (Infix _ n) = Infix ALeft (n + 1)
    raisePrec x = x
    readOneOper :: Text → LoadMonad ()
    readOneOper "r" = heading . _Left %= reassoc
    readOneOper operName = do
      use heading >>= flip either
        (error "Right header in operator paragraph")
        (\operFixity → fixity %= mappend (M.singleton operName operFixity))
  in
    doOrFinish $ \line → do
    traverse readOneOper (T.words line)
    heading . _Left %= raisePrec

consumeRule :: LoadMonad ()
consumeRule =
  doOrFinish $ \line → do
  flip recParser line <$> use fixity >>=
    \case
      Left e → do
        l ← use lineNumber
        throwError $ RuleFieldError l e
      Right x → do
        currentRuleContext %= (x:)
      

loadRule :: RuleName → LoadMonad ()
loadRule ruleName = do
  fmap uncons (use currentRuleContext) >>=
    \case
      Nothing → pure ()
      Just (conclusion, premises) →
        tell $ M.singleton ruleName (Rule conclusion $ reverse premises)
  currentRuleContext .= []

finishParagraph :: LoadMonad ()
finishParagraph = do
  inParagraph .= False
  use heading >>= either (const (pure ())) loadRule
