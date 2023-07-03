{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module React where

import Prelude hiding ((!!))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Javascript.JSaddle hiding (Ref)
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.String

#ifndef ghcjs_HOST_OS
import GHCJS.Prim.Internal (primToJSVal)
import Data.Coerce (coerce)
#else
import GHCJS.Foreign.Callback
import qualified JavaScript.Array as Array (toListIO, fromListIO)
#endif

t :: Text -> Text
t = id

tshow :: Show a => a -> Text
tshow = T.pack . show

#ifndef ghcjs_HOST_OS
printJavaScriptException :: JavaScriptException -> JSM ()
printJavaScriptException (JavaScriptException e) = do
  s <- e # t "toString" $ ()
  j <- valToJSON s
  liftIO $ T.putStrLn $ "Exception: " <> tshow j
#endif

#ifndef ghcjs_HOST_OS
instance PToJSVal Text where
  pToJSVal s = primToJSVal $ PrimVal_String s

instance PToJSVal Int where
  pToJSVal i = primToJSVal $ PrimVal_Number $ fromIntegral i
#endif

instance IsString JSVal where
  fromString = pToJSVal . T.pack

instance ToJSVal v => ToJSVal (Map Text v) where
  toJSVal m = do
    o@(Object oVal) <- obj
    forM_ (Map.toList m) $ \(k, v) -> do
      (o <# k) =<< toJSVal v
    pure oVal

consoleLog :: ToJSVal a => a -> JSM JSVal
consoleLog x = (global ! t "console") # t "log" $ [x]

instance ToJSVal (Component props refVal) where
  toJSVal (Component f) = toJSVal f

instance PToJSVal (Component props refVal) where
  pToJSVal (Component f) = pToJSVal f

instance PToJSVal Function where
  pToJSVal (Function _ o) = pToJSVal o

instance PToJSVal Object where
  pToJSVal (Object v) = v

newtype Component props refVal = Component { unComponent :: Function' }

newtype Hook a = Hook { unHook :: ReaderT React JSM a }
  deriving (Functor, Applicative, Monad)

newtype Render a = Render { unRender :: ReaderT React JSM a }
  deriving (Functor, Applicative, Monad)

-- | An object that contains the React library
newtype React = React { unReact :: Object }

instance MakeObject React where
  makeObject = pure . unReact

instance MakeObject (Component props refVal) where
  makeObject = makeObject . functionObject' . unComponent

newtype Element = Element { unElement :: ReaderT React JSM JSVal }

instance IsString Element where
  fromString = Element . pure . pToJSVal . T.pack

newtype Tag = Tag { unTag :: JSVal }

instance IsString Tag where
  fromString = Tag . pToJSVal . T.pack

createElement :: Tag -> Map Text JSVal -> [Element] -> Element
createElement etag props children = Element $ do
  react <- ask
  createdChildren <- mapM unElement children
  lift $ react # t "createElement" $ [pure $ unTag etag, toJSVal props] <> fmap pure createdChildren

createFragment :: [Element] -> Element
createFragment = createFragmentWithProps mempty

createFragmentWithProps :: Map Text JSVal -> [Element] -> Element
createFragmentWithProps props children = Element $ do
  react <- ask
  fragmentTag <- lift $ fmap Tag $ react ! t "Fragment"
  unElement $ createElement fragmentTag props children

--TODO: The Hook section shouldn't have any control flow to it; probably it also shouldn't depend on props except in specific ways
component :: Hook (JSVal -> Render Element) -> ReaderT React JSM (Component JSVal ())
component (Hook hook) = do
  react <- ask
  f <- lift $ function' $ \_ _ args -> flip runReaderT react $ do
    render <- hook
    let props = case args of
          [] -> jsUndefined
          arg0 : _ -> arg0
    e <- unRender $ render props
    unElement e
  pure $ Component f

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

-- TODO: Add the following in jsaddle
type JSCallAsFunction' = JSVal      -- ^ Function object
                     -> JSVal      -- ^ this
                     -> [JSVal]    -- ^ Function arguments
                     -> JSM JSVal  -- ^ Return value

function' :: JSCallAsFunction' -- ^ Haskell function to call
         -> JSM Function'     -- ^ Returns a JavaScript function object that will
                             --   call the Haskell one when it is called
#ifdef ghcjs_HOST_OS
function' f = do
    callback <- syncCallback2' $ \this args -> do
        rargs <- Array.toListIO (coerce args)
        f this this rargs -- TODO pass function object through
    Function' callback <$> makeFunctionWithCallback' callback
#else
function' f = do
    (cb, f') <- newSyncCallback'' f --TODO: "ContinueAsync" behavior
    return $ Function' cb $ Object f'
#endif

#ifdef ghcjs_HOST_OS
data Function' = Function' {functionCallback' :: Callback (JSVal -> JSVal -> IO JSVal), functionObject' :: Object}
#else
data Function' = Function' {functionCallback' :: CallbackId, functionObject' :: Object}
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$r = function () { return $1(this, arguments); }"
    makeFunctionWithCallback' :: Callback (JSVal -> JSVal -> IO JSVal) -> IO Object
#endif

instance ToJSVal Function' where
    toJSVal = toJSVal . functionObject'

instance PToJSVal Function' where
  pToJSVal (Function' _ o) = pToJSVal o
