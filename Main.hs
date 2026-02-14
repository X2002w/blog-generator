-- Main.hs
module Main where

import qualified Markup
import qualified Html
import Convert (convert)

import System.Directory (doesFileExist)
import System.Environment (getArgs)


main :: IO ()
main = do
  getArgs >>= \args ->
    case args of
      -- 无程序参数, 从stdin读, 写入stdout
      [] ->
        getContents >>= \content ->
          putStrLn (process "Empty title" content)
      
      -- 带有输入, 输出文件参数
      [input, output] ->
        readFile input >>= \content ->
          doesFileExist output >>= \exists ->
            let
              writeResult = writeFile output (process input content)
            in
              if exists
                then whenIO confirm writeResult
                else writeResult
      
      -- 其他情况
      _ -> 
        putStrLn "Usage: runghc Main.hs [-- <input file> <output file>]"


-- Html.Title -> 网页标题 
-- String -> 待解析文本
process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse


confirm :: IO Bool
confirm = 
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ -> putStrLn "Invalid response. use y or n" *>
          confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = 
  cond >>= \result ->
    if result 
      then action
      else pure ()