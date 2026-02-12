data Brightness = Dark | Light
data EightColor  
  = Black 
  | Red 
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor = AnsiColor Brightness EightColor

isBright :: AnsiColor -> Bool
isBright ansiColor = 
  case ansiColor of
    AnsiColor Dark _ -> False
    AnsiColor Light _ -> True

ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor = 
  case ansiColor of
    AnsiColor brightness color ->
      case brightness of
        Dark ->
          case color of
            Black -> UbuntuBlack
            Red -> UbuntuRed
            Green -> UbuntuGreen
            Yellow -> UbuntuYellow
            Blue -> UbuntuBlue
            Magenta -> UbuntuMagenta
            Cyan -> UbuntuCyan
            White -> UbuntuWhite
        Light -> 
          case color of
            Black -> UbuntuBrightBlack
            Red -> UbuntuBrightRed
            Green -> UbuntuBrightGreen
            Yellow -> UbuntuBrightYellow
            Blue -> UbuntuBrightBlue
            Magenta -> UbuntuBrightMagenta
            Cyan -> UbuntuBrightCyan
            White -> UbuntuBrightWhite

isEmpty :: [a] -> Bool
isEmpty list = 
  case listToMaybe list of
    Nothing -> True
    Just _ -> False         


isEmpty :: [a] -> Bool
isEmpty list = 
  case list of
    [] -> True
    _ : _ -> False