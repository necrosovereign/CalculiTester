{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveFunctor #-}

module TypeDef where

import Text.Parsec.Error (ParseError)
import Data.Text.Lazy (Text)
import Data.Map (Map)

data Rec a = Var a |
             Atom a [Rec a]
             deriving(Eq, Show, Read, Functor)

data MemCell a = Val (Rec a) | EqClass Int deriving(Eq, Show, Read, Functor)

type Store a = Map a (MemCell a)

data UnifierError a = NotInitialized a |
                      IncompabitibleAtoms a a |
                      ConflictingArity a |
                      CyclicVar a
                      deriving(Eq, Show, Read, Functor)

data CTParserError = TokenizerError Int |
                     CTParserError ParseError
                   deriving(Eq, Show)

type LineNumber = Int

data FileReaderError = RuleFieldError LineNumber CTParserError
                       deriving(Eq, Show)

data Token a = VarT a |
               AtomT a |
               OperT a |
               SymbolT Char
               deriving(Eq,Show,Read)

type Prec = Int

data Assoc = ALeft | ARight deriving(Eq, Show, Read)

data OperType = Infix Assoc Prec |
                Prefix |
                Postfix
                deriving(Eq, Show, Read)

type Fixity = Map Text OperType

type RuleName = Text

data Rule a = Rule (Rec a) [Rec a]
              deriving(Eq, Show, Read, Functor)

type Theory a = (Fixity, Map RuleName (Rule a))
