{-# LANGUAGE UnicodeSyntax #-}

module TypeDef where

data Rec a = Var a |
             Atom a [Rec a]
             deriving(Eq, Show, Read)

data UnifierError a = NotInitialized a |
                      IncompabitibleAtoms a a |
                      ConflictingArity a |
                      CyclicVar a
                      deriving(Eq, Show, Read)

data CTParserError = TokenizerError Int
                       deriving(Eq, Show, Read)

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
