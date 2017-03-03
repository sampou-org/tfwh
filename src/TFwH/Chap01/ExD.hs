module TFwH.Chap01.ExD where

import Data.Char (toLower)
import Prelude hiding (Word)

type Text = String
type Word = String

words'  :: Text -> [Word]
words'  = words . map toLower

words'' :: Text -> [Word]
words'' = map (map toLower) . words

-- ^
-- >>> let sample = "To be or not to be"
-- >>> words' sample
-- ["to","be","or","not","to","be"]
-- >>> words'' sample
-- ["to","be","or","not","to","be"]

-- prop> words' xs == words'' xs

