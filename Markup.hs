-- Markup.hs

module Markup 
  ( Document
  , Structure(..)
  , parse
  )
where

import Numeric.Natural
import Data.Maybe (maybeToList)
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- maybeToList :: Maybe a -> [a]
-- maybeToList Nothing = []
-- maybeToList (Just x) = [x]

type Document 
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  -- 为上述所有构造器添加自动生成 类型类实例
  deriving (Eq, Show)
-- 类似于
-- instance Eq Structure where
--   (Heading n1 s1) == (Heading n2 s2) = n1 == n2 && s1 == s2
--   (Paragraph s1) == (Paragraph s2) = s1 == s2
--   ...
-- instance Show Structure where
--   show (Heading n s) = "Heading " ++ show n ++ " " ++ show s
--   show (Paragraph s) = "Paragraph " ++ show s

parse :: String -> Document
parse = parseLines Nothing . lines 

-- context: 当前正在处理的结构
-- txts: 剩余的文本行
parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts = 
  case txts of
    -- texts 为空时, 若context有值, 则将其加入结果列表, 否则返回空列表(Nothing)
    [] -> maybeTolist context

    -- Heading 1 case: 当前行以 '*' 开头, 之后跟一个空格, 然后是标题文本
    ('*' : ' ' : line) : rest ->
      (maybe id (:) context) (Heading 1 (trim line) : parseLines Nothing rest) 
      -- 解析当前行, 将其转换为 Heading 1 结构, 然后递归处理剩余文本
      -- 相当于
      -- case context of
      --   Nothing -> (Heading 1 (trim line) : parserLiens Nothing rest)
      --   Just ctx -> ctx : (Heading 1 (trim line) : parserLiens Nothing rest)

    ('-' : ' ' : line) : rest -> 
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest 
        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) 
            rest )
    
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest 
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) 
            rest )
    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words