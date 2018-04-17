{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}

module CTParser (recParser) where

import TypeDef
import CTTokenizer
import Data.Char
import Text.Parsec
import Data.Maybe
import Data.Either
import Data.Text.Lazy (Text)
import qualified Data.Map as M

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
varT = tokenT isVar <?> "variable" where
  isVar (VarT s) = Just s
  isVar _ = Nothing

-- atom token
atomT :: ReadP String
atomT = tokenT isAtom <?> "atom" where
  isAtom (AtomT s) = Just s
  isAtom _ = Nothing

-- reserved symbols tokens
resSymbT :: Char → ReadP Char
resSymbT c = tokenT isSymb <?> [c] where
  isSymb (SymbolT s) | c == s = Just c
  isSymb _ = Nothing

-- parses infix operator with fixity greater or equal to n
infixOperT :: Prec → ReadP (Assoc, Prec, String)
infixOperT n = do
  fixity ← getState
  let isInfix t = case t of
        OperT s → case M.lookup s fixity of
          Just (Infix a m) | m >= n → Just (a, m, s)
          _ → Nothing
        _ → Nothing
  tokenT isInfix <?> "operator with precedence at least " ++ show n

-- parsers for prefix and postfix operators
prefixOperT :: ReadP String
prefixOperT = do
  fixity ← getState
  let isPrefix t = case t of
        OperT s → case M.lookup s fixity of
          Just Prefix → Just s
          _ → Nothing
        _ → Nothing
  tokenT isPrefix <?> "prefix operator"
        

postfixOperT :: ReadP String
postfixOperT = do
  fixity ← getState
  let isPostfix t = case t of
        OperT s → case M.lookup s fixity of
          Just Postfix → Just s
          _ → Nothing
        _ → Nothing
  tokenT isPostfix <?> "postfix operator"

-- parses a nucleus: variable, atom or a record in parenthesis
nucleusP :: ReadP (Rec String)
nucleusP = choice
  [ fmap Var varT
  , fmap (\x → Atom x []) atomT
  , listP
  , between (resSymbT '(') (resSymbT ')') recordP
  ]
  <?> "nucleus"

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
  <?> "simple record"
  

listP :: ReadP (Rec String)
listP = do
  elems ← between (resSymbT '[') (resSymbT ']') $
    recordP `sepBy` resSymbT ';'
  pure $ foldr (\x y → Atom "#" [x, y]) (Atom "[]" []) elems

-- parses expression on precedence level n
recordPrecP :: Prec → ReadP (Rec String)
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
recordP :: ReadP (Rec String)
recordP = recordPrecP 0

recParser :: Fixity → Text → Either CTParserError (Rec String)
recParser fixity s = do
  ts ← tokenizer s
  either (Left . CTParserError) Right $
    runParser recordP fixity "" ts
