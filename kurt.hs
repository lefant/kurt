{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

import KurtLib()

main :: IO ()
main =
    interact (unlines . lines)
