{-# LANGUAGE CPP #-}
module React.Types where

import Control.Monad.Reader
import Data.String
import qualified Data.Text as T
import Language.Javascript.JSaddle hiding (Ref)

import React.JSaddle

-- | An object that contains the React library
newtype React = React { unReact :: Object }

instance MakeObject React where
  makeObject = pure . unReact

newtype Component props refVal = Component { unComponent :: Function' }
  deriving (ToJSVal, PToJSVal)

instance MakeObject (Component props refVal) where
  makeObject = makeObject . functionObject' . unComponent

newtype Hook a = Hook { unHook :: ReaderT React JSM a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadJSM
#ifndef ghcjs_HOST_OS
           , MonadIO
#endif
           )

newtype Element = Element { unElement :: ReaderT React JSM JSVal }

instance IsString Element where
  fromString = Element . pure . pToJSVal . T.pack

newtype Tag = Tag { unTag :: JSVal }

instance IsString Tag where
  fromString = Tag . pToJSVal . T.pack
