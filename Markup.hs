-- Markup.hs

module Markup 
  ( Document
  , Structure(..)
  )
where

import Numeric.Natural

type Document 
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]

parse :: String -> Document
-- lines :: String -> [String]
parse = parserLines [] . lines 

parserLines :: [String] -> [String] -> Document
parserLines currentParagraph txts = 
  let
    paragraph = Paragraph (unlines (reverse currentParagraph))
  in
    case txts of 
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parserLines [] rest
          else
            parserLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words