import Html

main :: IO ()
main = putStrLn (renderHtml myhtml)

myhtml :: Html
myhtml = 
  html_ 
    "my title"
    (
        (h1_ "hello, weg <> generator")
        <>
        (
            (p_ "this is my webpage.")
            <>
            (p_ "welcome to visit!")
        )
    )
