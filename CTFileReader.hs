{-# LANGUAGE UnicodeSyntax #-}

module CTFileReader (loadTheory) where

import CTParser

loadTheory :: Filename → IO Theory
