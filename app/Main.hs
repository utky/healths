module Main where

import Healths.Server (start)

main :: IO ()
main = start 8080 "data.json"
