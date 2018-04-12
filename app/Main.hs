module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  n <- getArgs
  let option1 = n !! 0
      option2 = read $ n !! 1
      option3 = case n !! 2 of
                  "forever" -> Nothing
                  _ -> Just $ read $ n !! 2
      option4 = if length n == 3
                   then False
                   else case n !! 3 of
                          "numbers" -> True
                          _ -> False
  crawl option1 option2 option3 option4

