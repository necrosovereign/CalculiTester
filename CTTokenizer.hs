{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}

module CTTokenizer (tokenizer) where

import TypeDef
import Data.Char 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
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
tailMuncher :: ReadP Text
tailMuncher = fmap T.pack . many . satisfy $ not . isDelim

-- parses token using the predicate on the initial symbol
tokenT :: (Char → Bool) → ReadP Text
tokenT p = do
  -- first character satisfies the predicate
  init ← satisfy p
  rest ← tailMuncher
  spaces
  pure $ T.cons init rest

-- variable token
varT :: ReadP Text
varT = tokenT isUpper 

-- atom token
atomT :: ReadP Text
atomT = tokenT isAtomInit 

-- reserved symbols tokens
resSymbT :: ReadP Char
resSymbT = do
  c ← oneOf specialChars
  spaces
  pure c

-- operator token
operT :: ReadP Text
operT =
  tokenT isOperSymb 

tokenizer :: Text → Either CTParserError [(SourcePos, Token Text)] 
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
