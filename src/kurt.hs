{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Kurt
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

-}



import Kurt.MainLoop (startLoop)

main :: IO ()
main = do
  startLoop
