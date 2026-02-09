import Prelude hiding (const) 

increment n = n + 1
decrement n = n - 1

const a b = a

add n m =
  if m /= 0
    then add (increment n) (decrement m)
    else n

main = 
  if const (add 3 2) (decrement 3) == 5
    then putStrLn "Yes."
    else putStrLn "No."