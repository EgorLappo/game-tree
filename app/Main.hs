{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Directory
import Control.Monad
import Data.List.Split

import Lib

main :: IO ()
main = let datadir = "data"
           countdir = "counts"
           n = 3
       in do 
            files <- listDir datadir -- list dir
            createDirectoryIfMissing True countdir
            countdirs <- forM files $ \f -> do
                let cd = countdir ++ "/"++ fileName f ++ "/"
                createDirectoryIfMissing True cd
                return cd
            return ()
            
            forM_ (zip files countdirs) $ \(f,d) -> do
              tree <- buildTreeFromFile n (datadir ++ "/" ++ f)
              writeCountMap d $ traverseTreeCounts tree "cnts"
            
            
-- get the name with no extensions   
fileName :: String -> String
fileName = head . splitOn "."

listDir :: FilePath -> IO [FilePath]
listDir dir = do
    files <- listDirectory dir
    return $ filter (\s -> head s /= '.') files