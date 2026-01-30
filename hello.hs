main = putStrLn myhtml
myhtml = makeHtml 
  "my title" 
  (h1_ "hello, world!" <> p_ "this is webpage")
makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

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

p_ :: String -> String
p_ = el "p"

el :: String -> String -> String
el tag content = 
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

