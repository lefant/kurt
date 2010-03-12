{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : TraceOrId
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

The Point of this module is to have a single place to globally enable
/ disable tracing.

-}

module Debug.TraceOrId (trace) where

import qualified Debug.Trace as D (trace)

trace :: String -> a -> a
trace = D.trace
-- trace _ = id
