module Main where

import Tr

main :: IO ()
main = putStrLn $ tr "eo" (Just "oe") "hello"
-- main = putStrLn $ tr "e" Nothing "hello"
