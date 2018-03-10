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

data Token a = VarT a |
               AtomT a |
               OpenP |
               CloseP |
               OperT a
               deriving(Eq,Show,Read)

type Prec = Int

data OperType = Infix Prec |
                Prefix |
                Postfix
                deriving(Eq, Show, Read)
