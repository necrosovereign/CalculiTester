{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}

module CTParser where

import TypeDef
import CTTokenizer
import Data.Char
import Text.Parsec


type Fixity = String → OperType

type ReadP = Parsec [(SourcePos, Token String)] Fixity

-- token parsers
-- a token is either varible, atom, operator or a parenthesis

-- basic token parser
tokenT :: (Token String → Maybe a) → ReadP a
tokenT p = token showTok fst (p . snd) where
  showTok t = case snd t of
    VarT s → s
    AtomT s → s
    SymbolT c → [c]
    OperT s → s

-- variable token
varT :: ReadP String
varT = tokenT isVar <?> "varT" where
  isVar (VarT s) = Just s
  isVar _ = Nothing

-- atom token
atomT :: ReadP String
atomT = tokenT isAtom <?> "atomT" where
  isAtom (AtomT s) = Just s
  isAtom _ = Nothing

-- reserved symbols tokens
resSymbT :: Char → ReadP ()
resSymbT c = tokenT isSymb <?> "resSymbT" where
  isSymb (SymbolT s) | c == s = Just c
  isSymb _ = Nothing

-- parses infix operator with fixity greater or equal to n
infixOperT :: Prec → ReadP (Assoc, Prec, String)
infixOperT n = do
  fixity ← getState
  let isInfix t = case t of
        OperT s → case fixity s of
          Infix a m | m >= n → Just (a, m, s)
          _ → Nothing
        _ → Nothing
  tokenT isInfix <?> "infixOperT"

-- parsers for prefix and postfix operators
prefixOperT :: ReadP String
prefixOperT = do
  fixity ← getState
  let isPrefix t = case t of
        OperT s → case fixity s of
          Prefix → Just s
          _ → Nothing
        _ → Nothing
  tokemT isPrefix <?> "prefixOperT"
        

postfixOperT :: ReadP String
postfixOperT = do
  fixity ← getState
  let isPostfix t = case t of
        OperT s → case fixity s of
          Prefix → Just s
          _ → Nothing
        _ → Nothing
  tokenT isPostfix <?> "postfixOperT"

-- parses a nucleus: variable, atom or a record in parenthesis
nucleusP :: ReadP (Rec String)
nucleusP = choice
  [ fmap Var varT
  , fmap (\x → Atom x []) atomT
  , listP
  , between leftParenT rightParenT recordP
  ]
  <?> "nucleusP"

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
simpleRecordP =
  Atom <$> atomT <*> (many shellP)
  <|>
  shellP
  <?> "nucleusP"
  

listP :: ReadP (Rec String)
listP = do
  elems ← between leftBracketT rightBracketT $
    recordP `sepBy` semicolT
  pure $ foldr (\x y → Atom "#" [x, y]) (Atom "[]" []) elems

-- parses expression on precedence level n
recordPrecP :: Prec → ReadP (Rec String)
recordPrecP n = do
  left ← simpleRecordP
  let go left = do
        oper ← infixOperT n
        case oper of
          Nothing → pure left
          Just oper → do
            fixity ← getState
            right ← case fixity oper of
              InfixL m → recordPrecP (m + 1)
              InfixR m → recordPrecP m
            go $ Atom oper [left, right]
  go left

-- full record parser
recordP :: ReadP (Rec String)
recordP = recordPrecP 0
