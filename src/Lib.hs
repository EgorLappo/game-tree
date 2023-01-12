{-# LANGUAGE OverloadedStrings #-}

module Lib (Tree, CountMap, buildTreeFromFile, traverseTreeCounts, writeCountMap) where

import qualified Data.ByteString.Lazy.Char8 as BS 
import Data.List (foldl', foldl1')
import qualified Data.HashMap.Strict as H
import Control.Monad (forM_)

data Tree = Tree { move   :: BS.ByteString
                 , count  :: Int
                 , leaves :: [Tree] }
  deriving (Show, Eq)

-- *** BUILDING THE GAME TREE ***

buildTreeFromFile :: Int -- tree depth
          -> FilePath -- file path
          -> IO Tree -- game tree with counts
buildTreeFromFile n = (foldl' traverseTreeBuild (Tree "start" 0 []) . map (getMoves n) . BS.lines <$>) . BS.readFile 

--base recursive function
traverseTreeBuild :: Tree -> [BS.ByteString] -> Tree
traverseTreeBuild (Tree m n ls) [] = Tree m (n+1) ls
traverseTreeBuild (Tree m n ls) (r:rs) = Tree m (n+1) ls' 
    where ls' | r `elem` map move ls = go r ls -- move already there -> go over leaves and modify the right one
              | otherwise = traverseTreeBuild (Tree r 0 []) rs : ls -- move not there -> append a new leaf to the tree

          go _ [] = []
          go r' (y:ys) | r' == move y = traverseTreeBuild y rs : ys -- right leaf -> kep inserting the rest to that leaf
                       | otherwise = y : go r' ys -- wrong leaf -> go to the next leaf

-- process pgn string (neatly formatted) 
-- by dropping the move numbers and splitting moves
getMoves :: Int -> BS.ByteString -> [BS.ByteString]
getMoves n = take n . dropEvery 3 . BS.words

-- drop every nth element starting by dropping first
dropEvery:: Int -> [a] -> [a]
dropEvery k = go k k
  where go _ _ []     = []
        go i n (x:xs) = if i == n
         then go 1 n xs
         else x : go (i+1) n xs



-- *** PRINTING THE RESULTS

newtype Counts = Counts { counts :: [(BS.ByteString, BS.ByteString)] }
  deriving (Show)
type CountMap = H.HashMap BS.ByteString Counts


extractCounts :: Tree -> Counts 
extractCounts t = Counts $ map (\m -> (move m, BS.pack $ show $ count m)) $ leaves t

traverseTreeCounts :: Tree -> BS.ByteString -> CountMap
traverseTreeCounts t pos 
  | null (leaves t) = H.empty -- we are at a leaf
  | otherwise = let curCounts = extractCounts t 
                    recurCounts = flip map (leaves t) $ \t' ->
                      traverseTreeCounts t' (BS.unwords [pos, move t'])
                in H.insert pos curCounts $ foldl1' H.union recurCounts

writeCountMap :: FilePath -> CountMap ->  IO ()
writeCountMap dir m = do
  forM_ (H.toList m) $ \(pos, cnt) -> 
    let file = dir ++ "/" ++ BS.unpack pos
        (names, nums) = unzip $ counts cnt
        cntStr = BS.unlines [BS.intercalate (BS.pack ", ") names, BS.intercalate (BS.pack ", ") nums]
    in BS.writeFile file cntStr
