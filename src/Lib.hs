module Lib where

import Data.ByteString.Lazy.Char8 as BS 
import Data.Text as T

data Tree = Tree T.Text [Tree] 

addRows :: Tree -> BS.ByteString -> Tree 
addRows t s = undefined

