{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}

module CTParser where

import TypeDef
import Data.Char
import Text.Parsec

type ReadP = Parsec String ()

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

-- parentheses tokens
leftParenT :: ReadP ()
leftParenT = char '(' *> pure ()

rightParenT :: ReadP ()
rightParenT = char ')' *> pure ()

-- operator token
operT :: ReadP String
operT =
  tokenT isOperSymb

tokenizer :: ReadP [Token String]
tokenizer =
  do
    spaces
    ts ← many $ choice
      [ fmap VarT varT
      , fmap AtomT atomT
      , fmap (const OpenP) leftParenT
      , fmap (const CloseP) rightParenT
      , fmap OperT operT]
    eof
    pure ts

type Fixity = String → OperType

-- parsers for operators of different fixity

-- parses infix operator with fixity greater or equal to n
infixOperT :: Fixity → Prec → ReadP (Maybe String)
infixOperT fixity n =try $ (
  do
    s ← operT
    case fixity s of
      Infix m | m >= n → pure (Just s)
      _ → fail ""
  )
  <|>
  pure Nothing

prefixOperT :: Fixity → ReadP String
prefixOperT fixity = try $ do
  s ← operT
  case fixity s of
    Prefix → pure s
    _ → fail ""

postfixOperT :: Fixity → ReadP String
postfixOperT fixity = try $ do
  s ← operT
  case fixity s of
    Postfix→ pure s
    _ → fail ""

-- full record parser
recordP :: ReadP (Rec String)
recordP = undefined

-- parses a nucleus: variable, atom or a record in parenthesis
nucleusP :: ReadP (Rec String)
nucleusP = choice
  [ fmap Var varT
  , fmap (\x → Atom x []) atomT
  , between leftParenT rightParenT recordP
  ]

-- parses a shell: a nucleus, surrounded by prefix and postfix operator
shellP :: Fixity → ReadP (Rec String)
shellP fixity = do
  prefs ← many (prefixOperT fixity)
  nucl ← nucleusP
  postf ← many (postfixOperT fixity)
  pure $
          foldr (\op x → Atom op [x])
          (foldl (\x op → Atom op [x]) nucl postf)
          prefs

-- parses a record without infix operators
simpleRecordP :: ReadP (Rec String)
simpleRecordP = undefined
