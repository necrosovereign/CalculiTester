{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}

module CTParser where

import TypeDef
import Data.Char
import Text.Parsec

type Fixity = String → OperType

type ReadP = Parsec String Fixity

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
isOperSymb c = not (isAlphaNum c || isDelim c || c == ';')

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
resSymbT :: Char → ReadP ()
resSymbT c = do
  char c
  spaces
  return ()

-- parentheses for grouping
leftParenT :: ReadP ()
leftParenT = resSymbT '('

rightParenT :: ReadP ()
rightParenT = resSymbT ')'

-- brackets for lists
leftBracketT :: ReadP ()
leftBracketT = resSymbT '['

rightBracketT :: ReadP ()
rightBracketT = resSymbT ']'

-- semicolon separates list elements
semicolT :: ReadP ()
semicolT = resSymbT ';'

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



-- parses infix operator with fixity greater or equal to n
infixOperT :: Prec → ReadP (Maybe String)
infixOperT n =try $ (
  do
    fixity ← getState
    s ← operT
    case fixity s of
      Infix m | m >= n → pure (Just s)
      _ → fail ""
  )
  <|>
  pure Nothing

-- parsers for prefix and postfix operators
prefixOperT :: ReadP String
prefixOperT = try $ do
  fixity ← getState
  s ← operT
  case fixity s of
    Prefix → pure s
    _ → fail ""

postfixOperT :: ReadP String
postfixOperT = try $ do
  fixity ← getState
  s ← operT
  case fixity s of
    Postfix→ pure s
    _ → fail ""

-- parses a nucleus: variable, atom or a record in parenthesis
nucleusP :: ReadP (Rec String)
nucleusP = choice
  [ fmap Var varT
  , fmap (\x → Atom x []) atomT
  , between leftParenT rightParenT recordP
  ]

-- parses a shell: a nucleus, surrounded by prefix and postfix operator
shellP :: ReadP (Rec String)
shellP = do
  prefs ← many prefixOperT
  nucl ← nucleusP
  postf ← many postfixOperT
  pure $
          foldr (\op x → Atom op [x])
          (foldl (\x op → Atom op [x]) nucl postf)
          prefs

-- parses a record without infix operators
simpleRecordP :: ReadP (Rec String)
simpleRecordP = do
  constr ← atomT
  args ← many $ shellP
  pure $ Atom constr args
  
-- full record parser
recordP :: ReadP (Rec String)
recordP = undefined
