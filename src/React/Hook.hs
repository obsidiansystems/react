{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module React.Hook where

import Prelude hiding ((!!))

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Language.Javascript.JSaddle hiding (Ref)

import React.JSaddle
import React.Misc
import React.Types

--TODO: Input can be an initializer function rather than value
--TODO: `set` can take `a -> a` instead of `a`
useState :: (ToJSVal a, FromJSVal a) => a -> Hook (a, a -> JSM ())
useState initialValue = Hook $ do
  react <- ask
  initialJSVal <- lift $ toJSVal initialValue
  result <- lift $ (react # t "useState") initialJSVal
  Just s <- lift $ fromJSVal =<< result !! 0 --TODO: Exception handling
  setter <- lift $ result !! 1
  pure
    ( s
    , \v' -> void $ call setter nullObject [v']
    )

useRef :: JSVal -> Hook JSVal
useRef initialValue = Hook $ do
  react <- ask
  lift $ (react # t "useRef") initialValue

useEffect :: (JSVal -> JSVal -> [JSVal] -> JSM JSVal) -> Maybe [JSVal] -> Hook ()
useEffect f deps = Hook $ do
  react <- ask
  Function' _ cb <- lift $ function' f
  depsArg <- case deps of
    Nothing -> pure []
    Just someDeps -> do
      depsArray <- lift $ toJSVal someDeps
      pure [depsArray]
  _ <- lift $ (react # t "useEffect") $ [pToJSVal cb] <> depsArg
  pure ()

useMemo :: (ToJSVal a, FromJSVal a) => JSM a -> Maybe [JSVal] -> Hook a
useMemo a deps = Hook $ do
  react <- ask
  Function' _ cb <- lift $ function' $ \_ _ _ -> toJSVal =<< a
  depsArg <- case deps of
    Nothing -> pure []
    Just someDeps -> do
      depsArray <- lift $ toJSVal someDeps
      pure [depsArray]
  resultVal <- lift $ (react # t "useMemo") $ [pToJSVal cb] <> depsArg
  Just result <- lift $ fromJSVal resultVal
  pure result

useCallback :: ToJSVal result => (JSVal -> JSVal -> [JSVal] -> JSM result) -> Maybe [JSM JSVal] -> Hook JSVal
useCallback f deps = Hook $ do
  react <- ask
  Function' _ cb <- lift $ function' $ \fObj this args -> toJSVal =<< f fObj this args
  depsArg <- case deps of
    Nothing -> pure []
    Just someDeps -> do
      depsArray <- lift $ toJSVal =<< sequence someDeps
      pure [depsArray]
  lift $ (react # t "useCallback") $ [pToJSVal cb] <> depsArg

--------------------------------------------------------------------------------
-- Not yet supported
--------------------------------------------------------------------------------

type DispatchFunction a = a -> Effect ()

type Reducer s a = s -> a -> s

useContext :: Context a -> Hook a
useContext = undefined

data Context a

createContext :: a -> IO (Context a)
createContext = undefined

provider :: Context a -> a -> Render b -> Render b
provider = undefined

data Ref a

forwardRef :: (props -> Ref refVal -> Hook (Render ())) -> Component props refVal
forwardRef = undefined

useImperativeHandle :: Ref a -> Effect a -> Maybe [JSVal] -> Hook ()
useImperativeHandle = undefined

useReducer :: Reducer s a -> a -> Maybe (a -> a) -> Hook (a, DispatchFunction a)
useReducer = undefined

useTransition :: Hook (Bool, Effect () -> Effect ())
useTransition = undefined

useDeferredValue :: a -> Hook a
useDeferredValue = undefined

useDebugValue :: a -> Maybe (a -> b) -> Hook ()
useDebugValue = undefined

useId :: Hook Text
useId = undefined

useSyncExternalStore :: (IO () -> IO (IO ())) -> IO a -> Maybe (IO a) -> Hook ()
useSyncExternalStore = undefined

newtype Effect a = Effect { unEffect :: JSM a }
  deriving (Functor, Applicative, Monad)
