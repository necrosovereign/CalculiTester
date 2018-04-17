{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module CTFileReader (loadTheory) where

import TypeDef
import CTParser
import Data.List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Map (Map)
import qualified Data.Map as M

loadTheory :: FilePath → IO (Theory String)
loadTheory filename = 
  fmap (loadTheoryList . T.lines) $ T.readFile filename

