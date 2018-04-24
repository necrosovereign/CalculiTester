{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}

module CTTokenizer (tokenizer) where

import TypeDef
import Data.Char 
import Data.Text.Lazy
import Text.Parsec

type ReadP = Parsec Text ()

-- token parsers
-- a token is either varible, atom, operator or a parenthesis

specialChars = "()[];"

-- predicate for delimiting characters
isDelim :: Char → Bool
isDelim c = isSpace c || c `elem` specialChars

-- predicate for atom starters
isAtomInit :: Char → Bool
isAtomInit c = isAlphaNum c && not (isUpper c)

--predicate for operator starters
isOperSymb :: Char → Bool
isOperSymb c = not (isAlphaNum c || isDelim c)

-- collects tail of a token
tailMuncher :: ReadP String
tailMuncher = many $ satisfy $ not . isDelim

-- parses token using the predicate on the initial symbol
tokenT :: (Char → Bool) → ReadP String
tokenT p = do
  -- first character satisfies the predicate
  init ← satisfy p
  rest ← tailMuncher
  spaces
  pure $ init : rest

-- variable token
varT :: ReadP String
varT = tokenT isUpper 

-- atom token
atomT :: ReadP String
atomT = tokenT isAtomInit 

-- reserved symbols tokens
resSymbT :: ReadP Char
resSymbT = do
  c ← oneOf specialChars
  spaces
  pure c

-- operator token
operT :: ReadP String
operT =
  tokenT isOperSymb 

tokenizer :: Text → Either CTParserError [(SourcePos, Token String)] 
tokenizer s = case parse tokenizer' "" s of
  Right ts → Right ts
  Left err → Left $ TokenizerError $ sourceColumn (errorPos err)
  where tokenizer' =
          do
            spaces
            ts ← many $
              (,) <$>
              getPosition <*>
              choice
              [ fmap VarT varT
              , fmap AtomT atomT
              , fmap SymbolT resSymbT
              , fmap OperT operT]
            eof
            pure ts
