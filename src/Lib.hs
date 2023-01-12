module Lib where

import qualified Data.ByteString.Lazy.Char8 as BS 
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO

data Tree = Tree { move   :: BS.ByteString
                 , count  :: Int
                 , leaves :: [Tree] }

processFile :: Int -- tree depth
          -> FilePath -- file path
          -> IO Tree -- game tree with counts
processFile n = (foldl (traverseTree) (Tree (BS.pack "start") 0 []) . map (getMoves n) . BS.lines <$>) . BS.readFile 

processRow :: Int -- tree depth
           -> Tree -- curent game tree
           -> BS.ByteString -- pgn string
           -> Tree 
processRow n t pgn = traverseTree t moves
    where moves = getMoves n pgn

--base recursive function
traverseTree :: Tree -> [BS.ByteString] -> Tree
traverseTree (Tree m n ls) [] = Tree m (n+1) ls
traverseTree (Tree m n ls) (r:rs) = Tree m (n+1) ls' 
    where ls' | r `elem` map move ls = go r ls -- move already there -> go over leaves and modify the right one
              | otherwise = traverseTree (Tree r 0 []) rs : ls -- move not there -> append a new leaf to the tree

          go _ [] = []
          go r' (y:ys) | r' == move y = traverseTree y rs : ys -- right leaf -> kep inserting the rest to that leaf
                       | otherwise = y : go r' ys -- wrong leaf -> go to the next leaf

-- process pgn string (neatly formatted) 
-- by dropping the move numbers and splitting moves
getMoves :: Int -> BS.ByteString -> [BS.ByteString]
getMoves n = take n . dropEvery 3 . BS.words

-- drop every nth element starting by dropping first
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) (drop 1 xs) ++ drop n (dropEvery n xs)

