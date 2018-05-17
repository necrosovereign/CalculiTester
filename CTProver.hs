{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Tree
import Control.Lens
import Control.Monad.State
import Unifier

type Namer = Map Text Int

stripDigits :: Text → Text
stripDigits = T.dropWhileEnd (isDigit)

newName :: MonadState s m => Lens' s Namer → Text → m Text
newName l templ' =
  let templ = stripDigits templ' in
  l . at templ %%= \case
    Nothing -> (templ, Just 0)
    Just n -> (T.append templ (T.pack (show n)), Just (n + 1))
