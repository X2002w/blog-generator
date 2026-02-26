module HsBlog
  ( main
  , process
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)


import System.Directory (doesFileExist)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    -- 无程序参数, 从stdin读, 写入stdout
    [] -> do
      content <- getContents
      putStrLn (process "Empty title" content)
    
    -- 带有输入, 输出文件参数
    [input, output] -> do
      content <- readFile input
      exists <- doesFileExist output
      let
        writeResult = writeFile output (process input content)

      if exists
        then whenIO confirm writeResult
        else writeResult
    
    -- 其他情况
    _ -> 
      putStrLn "Usage: runghc Main.hs [-- <input file> <output file>]"


-- Html.Title -> 网页标题 
-- String -> 待解析文本
process :: Html.Title -> String -> String
process title = Html.renderHtml . convert title . Markup.parse


confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)" 
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n" 
      confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result 
    then action
    else pure ()