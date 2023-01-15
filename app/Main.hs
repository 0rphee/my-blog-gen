module Main (main) where

import Html

skipLine :: IO ()
skipLine = putStrLn ""

------------------------
myhtml :: HTML
myhtml = html_ "My title" $
  h1_ "Heading" <> p_ "Paragraph #1"<> ul_ [p_ "First item", p_ "SecondItem"] <> 
  h1_ "Heading #2" <> p_ "Paragraph #2" <> ol_ [p_ "First item", p_ "SecondItem"]

someFunc :: IO ()
someFunc = print myhtml

main :: IO ()
main = do
  skipLine
  someFunc
  skipLine
