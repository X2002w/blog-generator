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
      -- 没有剩余文本了, 结束递归
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            -- 构建输出列表
            paragraph : parserLines [] rest
          else
            parserLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words