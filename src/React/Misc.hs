module React.Misc where

import Data.Text (Text)
import qualified Data.Text as T

t :: Text -> Text
t = id

tshow :: Show a => a -> Text
tshow = T.pack . show

