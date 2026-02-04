import Html

main :: IO ()
main = putStrLn (renderHtml myhtml)

myhtml :: Html
myhtml = 
  html_ 
    "my title"
    (
      append_ 
        (h1_ "hello, weg generator")
        (
          append_ 
            (p_ "this is my webpage.")
            (p_ "welcome to visit!")
        )
    )
