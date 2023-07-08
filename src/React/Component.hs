module React.Component where

import Control.Monad.Except
import Control.Monad.Reader
import Language.Javascript.JSaddle hiding (Ref)

import React.JSaddle
import React.Types

--TODO: The Hook section shouldn't have any control flow to it; probably it also shouldn't depend on props except in specific ways
component
  :: FromJSVal props
  => (props -> Hook Element)
  -> ReaderT React JSM (Component props ())
component hook = do
  react <- ask
  f <- lift $ function' $ \_ _ args -> flip runReaderT react $ do
    let propsVal = case args of
          [] -> jsUndefined
          arg0 : _ -> arg0
    props <- liftJSM $ fromJSVal propsVal >>= \case
      Nothing -> fail "Invalid props"
      Just props -> pure props
    e <- unHook $ hook props
    unElement e
  pure $ Component f

