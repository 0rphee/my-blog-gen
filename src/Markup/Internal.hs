{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Markup.Internal where

import Numeric.Natural
import Data.Maybe (maybeToList)

type Document = [Structure]

data Structure = Heading       Natural String
               | Paragraph     String
               | UnorderedList [String] 
               | OrderedList   [String]
               | CodeBlock     [String]



trim :: String -> String
trim = unwords . words

parseLines :: Maybe Structure -> [String] -> Document
parseLines context [] = maybeToList context
parseLines context (currLine:rest)  
  | line == "" = maybe id (:) context (parseLines Nothing rest)
  | otherwise = case context of 
                  Just (Paragraph paragraph) ->
                    parseLines (Just $ Paragraph (unwords [paragraph, line])) rest
                  _ -> 
                    maybe id (:) context (parseLines (Just $ Paragraph line) rest) 
  where line = trim currLine

parse :: String -> Document
parse = parseLines Nothing . lines
