{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module CalculiTester.Parser (recParser) where

import Data.Char
import Text.Parsec
import Data.Maybe
import Data.Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import CalculiTester.Tokenizer
import CalculiTester.TypeDef

type ReadP = Parsec [(SourcePos, Token Text)] Fixity

-- token parsers
-- a token is either varible, atom, operator or a parenthesis

-- basic token parser
tokenT :: (Token Text → Maybe a) → ReadP a
tokenT p = token showTok fst (p . snd) where
  showTok t = T.unpack $ case snd t of
    VarT s → s
    AtomT s → s
    SymbolT c → T.singleton c
    OperT s → s

-- variable token
varT :: ReadP Text
varT = tokenT isVar <?> "variable" where
  isVar (VarT s) = Just s
  isVar _ = Nothing

-- atom token
atomT :: ReadP Text
atomT = tokenT isAtom <?> "atom" where
  isAtom (AtomT s) = Just s
  isAtom _ = Nothing

-- reserved symbols tokens
resSymbT :: Char → ReadP Char
resSymbT c = tokenT isSymb <?> [c] where
  isSymb (SymbolT s) | c == s = Just c
  isSymb _ = Nothing

-- parses infix operator with fixity greater or equal to n
infixOperT :: Prec → ReadP (Assoc, Prec, Text)
infixOperT n = do
  fixity ← getState
  let isInfix t = case t of
        OperT s → case M.lookup s fixity of
          Just (Infix a m) | m >= n → Just (a, m, s)
          _ → Nothing
        _ → Nothing
  tokenT isInfix <?> "operator with precedence at least " ++ show n

-- parsers for prefix and postfix operators
prefixOperT :: ReadP Text
prefixOperT = do
  fixity ← getState
  let isPrefix t = case t of
        OperT s → case M.lookup s fixity of
          Just Prefix → Just s
          _ → Nothing
        _ → Nothing
  tokenT isPrefix <?> "prefix operator"
        

postfixOperT :: ReadP Text
postfixOperT = do
  fixity ← getState
  let isPostfix t = case t of
        OperT s → case M.lookup s fixity of
          Just Postfix → Just s
          _ → Nothing
        _ → Nothing
  tokenT isPostfix <?> "postfix operator"

-- parses a nucleus: variable, atom or a record in parenthesis
nucleusP :: ReadP (Rec Text)
nucleusP = choice
  [ fmap Var varT
  , fmap (\x → Atom x []) atomT
  , listP
  , between (resSymbT '(') (resSymbT ')') recordP
  ]
  <?> "nucleus"

-- parses a shell: a nucleus, surrounded by prefix and postfix operator
shellP :: ReadP (Rec Text)
shellP = do
  prefs ← many prefixOperT
  nucl ← nucleusP
  postf ← many postfixOperT
  pure $
    foldr (\op x → Atom op [x])
    (foldl (\x op → Atom op [x]) nucl postf)
    prefs

-- parses a record without infix operators
simpleRecordP :: ReadP (Rec Text)
simpleRecordP =
  Atom <$> atomT <*> (many shellP)
  <|>
  shellP
  <?> "simple record"
  

listP :: ReadP (Rec Text)
listP = do
  elems ← between (resSymbT '[') (resSymbT ']') $
    recordP `sepBy` resSymbT ';'
  pure $ foldr (\x y → Atom "#" [x, y]) (Atom "[]" []) elems

-- parses expression on precedence level n
recordPrecP :: Prec → ReadP (Rec Text)
recordPrecP n =
  flip ($) <$> simpleRecordP <*> go <?> "recordPrepP " ++ show n where
  go = (do
           (assoc, m, oper) ← infixOperT n
           right ← case assoc of
             ALeft → recordPrecP (m + 1)
             ARight → recordPrecP m
           f ← go
           pure (\left → f $ Atom oper [left, right])
       ) <|>
       pure id

-- full record parser
recordP :: ReadP (Rec Text)
recordP = recordPrecP 0

--the main function
recParser :: Fixity → Text → Either CTParserError (Rec Text)
recParser fixity s = do
  ts ← tokenizer s
  either (Left . CTParserError) Right $
    runParser recordP fixity "" ts
