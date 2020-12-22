module Main
       where

import System.IO

main = do
  hSetBuffering stdin LineBuffering
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ( "Hi, " ++ name ++ "." )
  
