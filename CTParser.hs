{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}

module CTParser where

import TypeDef
import Data.Char
import Text.ParserCombinators.ReadP

-- token parsers
-- a token is either varible, atom, operator or a parenthesis

-- predicate for delimiting characters
isDelim :: Char → Bool
isDelim c = isSpace c || c `elem` "()"

-- predicate for atom starters
isAtomInit :: Char → Bool
isAtomInit c = isAlphaNum c && not (isUpper c)

--predicate for operator starters
isOperSymb :: Char → Bool
isOperSymb c = not (isAlphaNum c || isDelim c)

-- collects tail of a token
tailMuncher :: ReadP String
tailMuncher = munch $ not . isDelim

-- parses token using the predicate on the initial symbol
tokenT :: (Char → Bool) → ReadP String
tokenT p = do
  skipSpaces
  -- first character satisfies the predicate
  init ← satisfy p
  rest ← tailMuncher
  skipSpaces
  pure $ init : rest

-- variable token
varT :: ReadP String
varT = tokenT isUpper

-- atom token
atomT :: ReadP String
atomT = tokenT isAtomInit

-- parentheses tokens
leftParenT :: ReadP ()
leftParenT = char '(' *> pure ()

rightParenT :: ReadP ()
rightParenT = char ')' *> pure ()

operT :: ReadP String
operT =
  tokenT isOperSymb

tokenizer :: ReadP [Token String]
tokenizer =
  do
    ts ← many $ choice
      [ fmap VarT varT
      , fmap AtomT atomT
      , fmap (const OpenP) leftParenT
      , fmap (const CloseP) rightParenT
      , fmap OperT operT]
    eof
    return ts
