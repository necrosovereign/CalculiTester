{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module CalculiTester.FileReader (loadTheory) where

import           CalculiTester.Parser
import           CalculiTester.TypeDef
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

--information about what is currently read
data Paragraph =
  --the paragraph describes operators
  --_operType contains fixity of currently read operators
  OperPar {_operType :: OperType } |
  --the paragraph describes a rule name _ruleName
  RulePar {_ruleName :: RuleName } |
  --the paragraph doesn't contain any information besides heading
  --used for reading the name of the theory
  OtherPar {_parName :: String}

makeLenses ''Paragraph

data LoadState = LoadState
  {
    --current line number for error reporting
    _lineNumber :: Int,
    --true initially and after empty line in a paragraph,
    --false after reading the heading
    _inParagraph :: Bool, 
    --contains fixities of the operators read so far
    _fixity :: Fixity,
    --the kind of the paragraph read currently
    _paragraph :: Paragraph,
    --an expressions' buffer to pack into a rule
    _currentRuleContext :: [Rec Text],
    --non-empty if the name of the theory has been read
    _theoryName :: String
  }

makeLenses ''LoadState

--Reader Monad on the file text,
--State Monad on the internal state (LoadState),
--Writer Monad on the theory
type LoadMonad = RWST [Text] (Theory Text) LoadState
  (Either FileReaderError)

--the main function, reads theory, its name and the fixity
--of the used operators, from the list of lines
loadTheory :: [Text] → Either FileReaderError ((String, Fixity), Theory Text)
loadTheory text = _Right . _1 %~ (\s → (view theoryName s, view fixity s)) $
  execRWST consumeLine text initLoadState 


initLoadState :: LoadState
initLoadState =
  LoadState {
  --start on line 1
  _lineNumber = 1,
    --file should start with a heading
    _inParagraph = False,
    --cons operator has predefined fixity
    _fixity = M.fromList [("#", Infix ARight 0)],
    _paragraph = undefined,
    _currentRuleContext = [],
    _theoryName = ""
  }

--loop that reads one line at a time
consumeLine :: LoadMonad ()
consumeLine =
  --finish if the input is empty
  asks null >>=
  \case
    True → finishParagraph
    False →
      do
        --uses consumeHeading, then uses consumeText
        --until the paragraph ends
        b ← use inParagraph
        if b
          then consumeText
          else consumeHeading
        --increment the line number
        lineNumber %= (+1)
        --recurse on the rest of the inpute
        local tail consumeLine

--called when the current line is the heading
--or an empty line before the heading
--inspects the line, and if it's the heading,
--writes the paragraph description
consumeHeading :: LoadMonad ()
consumeHeading = do
  --read the current line
  line ← asks head
  --type of the heading is determined
  --by the first two characters
  let (start, rulename) = T.splitAt 2 line
  --if the first two characters corresponds to a heading
  --record that the heading has been read
  when (start `elem` ["--", "in", "pr", "po", "na"]) $
    inParagraph .= True
  --determin the kind of the paragraph and record it
  paragraph .= (
    case start of
      --rule paragraph
      "--" → RulePar $ rulename
      --operators description paragraphs
      --infix operators start with left associativity
      --and the precedence 1
      "in" → OperPar $ Infix ALeft 1
      "pr" → OperPar $ Prefix
      "po" → OperPar $ Postfix
      --heading contains the name of the theory
      --characters "name: " are dropped
      --and the rest is the name
      "na" → OtherPar $ T.unpack . T.strip $ T.drop 6 line
      --not supposed to happen
      _ → OtherPar ""
    )

--checks the type of the paragraph and calls
--the function to consume a line of this type
consumeText :: LoadMonad ()
consumeText = do
  paragraphType ← use paragraph
  case paragraphType of
    --operator line consumer
    OperPar _ → consumeOper
    --rule line consumer
    RulePar _ → consumeRule
    --do nothing until the end of the paragraph
    OtherPar _ → doOrFinish (const $ pure ())

--takes a line consumer and calls the finisher,
--if the line is empty, otherwise
--feeds the line to the consumer
doOrFinish :: (Text → LoadMonad ()) → LoadMonad ()
doOrFinish consumer = do
  line ← fmap T.strip $ asks head
  if T.null line
    then finishParagraph
    else consumer line

--operator line consumer
consumeOper :: LoadMonad ()
consumeOper =
  let
    --changes associativity in the fixity to the right
    reassoc (Infix _ n) = Infix ARight n
    reassoc x = x
    --raises precedence by 1 and reassociate to the left
    raisePrec (Infix _ n) = Infix ALeft (n + 1)
    raisePrec x = x
    --function to read one word
    readOneOper :: Text → LoadMonad ()
    --"r" signals to reassociate fixity
    readOneOper "r" = paragraph . operType %= reassoc
    --otherwise read current fixity in
    --the paragraph and write it to the fixity map
    readOneOper operName =
      use paragraph >>=
      \case
        OperPar operFixity → fixity %=
          mappend (M.singleton operName operFixity)
        _ → error "Right header in operator paragraph"
  in
    doOrFinish $ \line → do
    --read each word in the line
    traverse readOneOper (T.words line)
    --then raise the precedence
    paragraph . operType %= raisePrec

--rule line consumer
consumeRule :: LoadMonad ()
consumeRule =
  doOrFinish $ \line → do
  -- parse the line with current fixity map
  flip recParser line <$> use fixity >>=
    \case
      --if parsing failed record the error
      Left e → do
        l ← use lineNumber
        throwError $ RuleFieldError l e
      --if successful add to the context
      Right x → do
        currentRuleContext %= (x:)
      
--packs the expressions' buffer into a rule named
--rulename
loadRule :: RuleName → LoadMonad ()
loadRule ruleName = do
  --separate conclusion from premises
  --since expressions are recorded in reverse order
  --the conclusion is the head of the list
  fmap uncons (use currentRuleContext) >>=
    \case
      --if rule doesn't have anything, do nothing
      Nothing → pure ()
      --Load Monad is a Writer for the Theory
      Just (conclusion, premises) →
        tell $ M.singleton ruleName
        --premises were recorded in reverse order,
        --so they are reversed back
        (Rule conclusion $ reverse premises)
  --clear the expressions' buffer
  currentRuleContext .= []

--operations after finishing reading a paragraph
finishParagraph :: LoadMonad ()
finishParagraph = do
  --signal the end of the paragraph
  inParagraph .= False
  --finish the current type of a paragraph
  use paragraph >>= \case
    --pack the expressions' buffer
    RulePar name → loadRule name
    --remember the name of the theory
    OtherPar name → theoryName .= name
    _ → pure ()
