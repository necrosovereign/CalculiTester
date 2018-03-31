{-# LANGUAGE UnicodeSyntax #-}

module TypeDef where

import Text.Parsec.Error (ParseError)
import Data.Map (Map)

data Rec a = Var a |
             Atom a [Rec a]
             deriving(Eq, Show, Read)

data UnifierError a = NotInitialized a |
                      IncompabitibleAtoms a a |
                      ConflictingArity a |
                      CyclicVar a
                      deriving(Eq, Show, Read)

data CTParserError = TokenizerError Int |
                     CTParserError ParseError
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

type Fixity = Map String OperType

type RuleName = String

data Rule a = Rule (Rec a) [Rec a]

type Theory a = (Fixity, Map RuleName (Rule a))
