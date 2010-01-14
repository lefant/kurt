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
                                       commandLoop
                                      ) where
    
import Network.GoTextProtocol2.Server.Parser

-- import System.IO.Utils
import System.IO.Error
import System.Log.Logger
import Data.Char
import Data.List
import System.IO

logname :: String
logname = "Network.GoTextProtocol2.Server"


type CommandHandler = String -> IO Bool
data Command = Command String CommandHandler

instance Eq Command where
    (Command x _) == (Command y _) = x == y
instance Ord Command where
    compare (Command x _) (Command y _) = compare x y
instance Show Command where
    show (Command cmd _) = cmd

commands :: [Command]
commands =
    [
     (Command "boardsize" cmd_boardsize)
    ,(Command "clear_board" cmd_clear_board)
    ,(Command "genmove" cmd_genmove)
    ,(Command "known_command" cmd_known_command)
    ,(Command "komi" cmd_komi)
    ,(Command "list_commands" cmd_list_commands)
    ,(Command "name" cmd_name)
    ,(Command "play" cmd_play)
    ,(Command "protocol_version" cmd_protocol_version)
    ,(Command "quit" cmd_quit)
    ,(Command "showboard" cmd_showboard)
    ,(Command "time_left" cmd_time_left)
    ,(Command "version" cmd_version)
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


lookupC :: String -> [Command] -> Maybe Command
lookupC cmd cl = find (\(Command x _) -> x == cmd) cl


cmd_known_command :: CommandHandler
cmd_known_command arg =
    do putStr "= "
       case lookupC arg commands of
         Nothing -> putStrLn "false"
         Just (Command _ _) -> putStrLn "true"
       putStrLn ""
       return True

cmd_list_commands :: CommandHandler
cmd_list_commands _ =
    do putStr "= "
       mapM print commands
       putStrLn ""
       return True

cmd_name :: CommandHandler
cmd_name _ =
    do putStrLn "= Kurt"
       putStrLn ""
       return True

cmd_protocol_version :: CommandHandler
cmd_protocol_version _ =
    do putStrLn "= 2"
       putStrLn ""
       return True

cmd_quit :: CommandHandler
cmd_quit _ =
    do putStrLn "= bye..."
       putStrLn ""
       return False

cmd_version :: CommandHandler
cmd_version _ =
    do putStrLn "= 0.0.1"
       putStrLn ""
       return True

-- TODO

cmd_time_left :: CommandHandler
cmd_time_left args =
    do putStrLn $ "time left: " ++ args
       putStrLn ""
       return True

cmd_clear_board :: CommandHandler
cmd_clear_board _ =
    do putStrLn "clear_board received"
       putStrLn ""
       return True

cmd_komi :: CommandHandler
cmd_komi _ =
    do putStrLn "clear_board received"
       putStrLn ""
       return True

cmd_play :: CommandHandler
cmd_play _ =
    do putStrLn "= "
       return True

cmd_genmove :: CommandHandler
cmd_genmove _ =
    do putStrLn "genmove received"
       putStrLn ""
       return True

cmd_boardsize :: CommandHandler
cmd_boardsize args =
    do putStrLn $ "boardsize " ++ args ++ " received"
       putStrLn ""
       return True

cmd_showboard :: CommandHandler
cmd_showboard _ =
    do putStrLn "showboard received"
       putStrLn ""
       return True

