{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module React.Export where

import Control.Monad.Reader as MTL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
import Language.Javascript.JSaddle

import React.Misc
import React.JSaddle ()
import React.Types

#ifndef ghcjs_HOST_OS
import Control.Monad.Except
import qualified Data.Text.IO as T
import Language.Javascript.JSaddle.Warp
#endif

mainExportsToJS :: [Name] -> Q [Dec]
mainExportsToJS names = [d|
    main :: IO ()
    main = exportToJSIO $ sequence $ Map.fromList $(listE $ fmap nameToExportEntry names)
  |]

nameToExportEntry :: Name -> Q Exp
nameToExportEntry n = [| (T.pack $(TH.lift $ nameBase n), MTL.lift . toJSVal =<< $(varE n)) |]

exportToJSIO :: ReaderT React JSM (Map Text JSVal) -> IO ()
exportToJSIO build = runJS $ \arg -> do
  react <- fmap (React . Object) $ arg ! t "react"
  m <- flip runReaderT react build
  _ <- (arg # t "setVal") [m]
  pure ()

runJS :: (JSVal -> JSM ()) -> IO ()

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "getProgramArg"
    getProgramArg :: JSM JSVal

runJS f = do
  arg <- getProgramArg
  f arg

#else

runJS f = do
  let port = 3001 --TODO: Get this from npm config or something
  run port $ \arg -> f arg `catchError` printJavaScriptException

printJavaScriptException :: JavaScriptException -> JSM ()
printJavaScriptException (JavaScriptException e) = do
  s <- e # t "toString" $ ()
  j <- valToJSON s
  liftIO $ T.putStrLn $ "Exception: " <> tshow j

#endif
