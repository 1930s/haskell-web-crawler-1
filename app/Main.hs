module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  n <- getArgs
  let option1 = read $ n !! 0
      option2 = case n !! 1 of
                  "forever" -> Nothing
                  _ -> read $ n !! 1
      option3 = if length n == 2
                   then False
                   else case n !! 2 of
                          "numbers" -> True
                          _ -> False
  crawl "Online_chat" option1 option2 option3

