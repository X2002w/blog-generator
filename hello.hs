newtype Html = Html String
newtype Structure = Structure String

getStructureString :: Structure -> String
getStructureString struct = 
  case struct of
    Structure str -> str

-- main = putStrLn myhtml

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_  = el "body"

head_ :: String -> String
head_  = el "head"

title_ :: String -> String
title_ = el "title"

h1_ :: String -> String
h1_ = el "h1"

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) :: (String -> Structure) -> (String -> String) -> (String -> Structure)
p_ :: String -> Structure
p_ = Structure . el "p"

el :: String -> String -> String
el tag content = 
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

myhtml = makeHtml 
  "my title" 
  (h1_ "hello, world!" <> p_ "this is webpage")
makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)


-- selfincr function
incr :: Int -> Int
incr x = x + 1
-- times2 function
times2 :: Int -> Int
times2 x = x * 2
-- selfincr and times2
addThenDouble :: Int -> Int
addThenDouble = times2 . incr

append_ :: Structure -> Structure -> Structure
append_ s1 s2 = 
  Structure (getStructureString s1 <> getStructureString s2)

main = print (addThenDouble 3)