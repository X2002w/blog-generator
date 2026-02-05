-- Html/Internal.hs

module Html.Internal where

 
newtype Html = Html String
newtype Structure = Structure String
type Title = String
-- html lable -> head lable | body lable
-- head lable -> title lable
-- 先生成body(Structure), 再结合head(title)
html_ :: Title -> Structure -> Html
html_  title content = 
  Html 
    (
      el "html"
        (
          el "head" (el "title" (escape title)) <>
          el "body" (getStructureString content)
        )
    )

getStructureString :: Structure -> String
getStructureString content = 
  case content of
    Structure str -> str

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape
-- 等价于 h1_ content = Structure (el "h1" (escape content))
-- h1_ = \content -> Structure (el "h1" content)

p_ :: String -> Structure
p_ = Structure . el "p" . escape
-- p_ = \content -> Structure (el "p" content)

ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

el :: String -> String -> String
el tag content = 
  "<" <> tag <> ">\n" <> content <> "</" <> tag <> ">\n"

append_ :: Structure -> Structure -> Structure
append_ s1 s2 = 
  Structure (getStructureString s1 <> getStructureString s2)
-- 另一种写法
-- append_ (Structure s1) (Structure s2) = 
--   Structure (s1 <> s2)

renderHtml :: Html -> String
-- renderHtml (Html str) = str
renderHtml content =
  case content of
    Html str -> str
 
escape :: String -> String 
escape = 
  let
    escapeChar c = 
      case c of 
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in 
    concat . map escapeChar