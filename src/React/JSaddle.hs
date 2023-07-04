{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Everything in this module belongs in JSaddle, GHCJS-DOM, or similar
module React.JSaddle where

import Prelude hiding ((!!))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Data.String

import React.Misc

#ifdef ghcjs_HOST_OS
import Data.Coerce (coerce)
import GHCJS.Foreign.Callback
import qualified JavaScript.Array as Array (toListIO)
import Language.Javascript.JSaddle
#else
import GHCJS.Prim.Internal (primToJSVal)
import Language.Javascript.JSaddle hiding (Ref)
#endif

#ifndef ghcjs_HOST_OS
instance PToJSVal Text where
  pToJSVal s = primToJSVal $ PrimVal_String s

instance PToJSVal Int where
  pToJSVal i = primToJSVal $ PrimVal_Number $ fromIntegral i
#endif

instance PToJSVal Function where
  pToJSVal (Function _ o) = pToJSVal o

instance PToJSVal Object where
  pToJSVal (Object v) = v

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
