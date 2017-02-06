module Main where

import Lib

main :: IO ()
main = do
        md <- readFile "test.md"
        sequence_ $ sequence . fmap print <$> getMails md
