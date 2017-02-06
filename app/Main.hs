module Main where

import Lib
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
          [fn] -> do
                  md <- readFile fn
                  sequence_ $ sequence . fmap print <$> getMails md
          _ -> do
                print "please call with markdown-file"
