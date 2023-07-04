{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module React.Component where

import Control.Monad.Except
import Control.Monad.Reader
import Language.Javascript.JSaddle hiding (Ref)

import React.JSaddle
import React.Types

--TODO: The Hook section shouldn't have any control flow to it; probably it also shouldn't depend on props except in specific ways
component :: (JSVal -> Hook Element) -> ReaderT React JSM (Component JSVal ())
component hook = do
  react <- ask
  f <- lift $ function' $ \_ _ args -> flip runReaderT react $ do
    let props = case args of
          [] -> jsUndefined
          arg0 : _ -> arg0
    e <- unHook $ hook props
    unElement e
  pure $ Component f

