{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{-
Copyright (C) 2010 Fabian Linzberger <e@lefant.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
-}

{- |
   Module     : Network.GoTextProtocol2.Server
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Nothing here yet, move on.

Written by Fabian Linzberger, e\@lefant.net
-}


module Network.GoTextProtocol2.Server (
                                       errorStubber
                                      ,commandLoop
                                      ) where
    
import Network.GoTextProtocol2.Server.Parser

-- import System.IO.Utils
import System.IO.Error
import System.Log.Logger
-- import Data.String.Utils
import Text.Printf
import Data.Char
import Data.IORef
import Data.List
import Control.Exception(finally)
import System.IO

logname = "Network.GoTextProtocol2.Server"


errorStubber = undefined

type CommandHandler = String -> IO Bool
data Command = Command String CommandHandler

instance Eq Command where
    (Command x _) == (Command y _) = x == y
instance Ord Command where
    compare (Command x _) (Command y _) = compare x y


commands :: [Command]
commands =
    [
     (Command "play" cmd_play)
    ,(Command "genmove" cmd_genmove)
    ,(Command "boardsize" cmd_boardsize)
    ]

commandLoop :: IO ()
commandLoop =
    let
        errorhandler e =
            do noticeM logname
                           ("Closing due to error: " ++ (show e))
               return False
    in
      do
        continue <- (flip catch) errorhandler
                    (do
                      x <- parseCommand stdin
                      case x of
                        Left err ->
                             do
                               putStrLn $ "Couldn't parse command: " ++ (show err)
                               return True
                        Right (cmd, args) -> 
                             case lookupC cmd commands of
                               Nothing ->
                                   do putStrLn $ "Unrecognized command " ++ cmd
                                      return True
                               Just (Command _ hdlr) -> hdlr args
                    )
        if continue
          then commandLoop
          else return ()


lookupC cmd cl = find (\(Command x _) -> x == cmd) cl

cmd_play :: CommandHandler
cmd_play args =
    do print "play received"
       return False

cmd_genmove :: CommandHandler
cmd_genmove args =
    do print "genmove received"
       return True

cmd_boardsize :: CommandHandler
cmd_boardsize args =
    do print "genmove received"
       return True

