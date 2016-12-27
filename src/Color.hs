module Color
  (red, green, blue, yellow, cyan
  ) where

colorify :: String -> Int -> String
colorify str digit = "\x1b[" ++ (show digit) ++ "m" ++ str ++ "\x1b[0m"
  
red :: String -> String
red str = colorify str 31

green :: String -> String
green str = colorify str 32
  
blue :: String -> String
blue str = colorify str 34

yellow :: String -> String
yellow str = colorify str 33

cyan :: String -> String
cyan str = colorify str 36
