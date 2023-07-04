module React.Element where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import Data.Text (Text)
import Language.Javascript.JSaddle hiding (Ref)

import React.Misc
import React.Types

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
