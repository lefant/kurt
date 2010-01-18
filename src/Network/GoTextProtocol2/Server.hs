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
import Network.GoTextProtocol2.Server.Types

-- import System.IO.Utils
import System.IO.Error
import Data.Char
import Data.List
import System.IO


type CommandHandler = [Argument] -> IO Bool

{-
instance Eq Command where
    (Command x _) == (Command y _) = x == y
instance Ord Command where
    compare (Command x _) (Command y _) = compare x y
instance Show Command where
    show (Command cmd _) = cmd
-}

lookupC :: String -> [(String, CommandHandler)] -> Maybe (String, CommandHandler)
lookupC cmd cl = find (\(x, _) -> x == cmd) cl


commandargparserlist =
    [
     ("boardsize", intArgParser)
    ,("clear_board", noArgumentParser)
    ,("genmove", colorArgParser)
    ,("known_command", stringArgParser)
    ,("komi", floatArgParser)
    ,("list_commands", noArgumentParser)
    ,("name", noArgumentParser)
    ,("play", moveArgParser)
    ,("protocol_version", noArgumentParser)
    ,("quit", noArgumentParser)
    ,("showboard", noArgumentParser)
    ,("time_left", intArgParser)
    ,("version", noArgumentParser)
    ]



commandHandlers :: [(String, CommandHandler)]
commandHandlers =
    [
     ("boardsize", cmd_boardsize)
    ,("clear_board", cmd_clear_board)
    ,("genmove", cmd_genmove)
    ,("known_command", cmd_known_command)
    ,("komi", cmd_komi)
    ,("list_commands", cmd_list_commands)
    ,("name", cmd_name)
    ,("play", cmd_play)
    ,("protocol_version", cmd_protocol_version)
    ,("quit", cmd_quit)
    ,("showboard", cmd_showboard)
    ,("time_left", cmd_time_left)
    ,("version", cmd_version)
    ]



commandLoop :: IO ()
commandLoop =
    let
        errorhandler e =
            do print ("Closing due to error: " ++ (show e))
               return False
    in
      do
        continue <- (flip catch) errorhandler
                    (do
                      x <- parseCommand stdin commandargparserlist
                      case x of
                        Left err ->
                             do
                               putStrLn $ "? Couldn't parse command: " ++ (show err)
                               return True
                        Right (maybeId, Command cmd args) ->
                             do
                               case lookupC cmd commandHandlers of
                                 Nothing ->
                                     do
                                       putStrLn $ "?" ++ (outputIdOrBlank maybeId) ++ "Unrecognized command " ++ cmd
                                       return True
                                 Just (_, hdlr) ->
                                     do
                                       putStrLn $ "now running handler for " ++ cmd
                                       -- FIXME: hdlr should be able to signal errors too
                                       putStr $ "=" ++ (outputIdOrBlank maybeId)
                                       hdlr args
                    )
        if continue
          then commandLoop
          else return ()


outputIdOrBlank :: Maybe Id -> String
outputIdOrBlank Nothing = " "
outputIdOrBlank (Just lineId) = "[" ++ (show lineId) ++ "] "

cmd_known_command :: CommandHandler
cmd_known_command [(StringArgument cmd)] =
    do
       case lookupC cmd commandHandlers of
         Nothing -> putStrLn "false"
         Just (_, _) -> putStrLn "true"
       return True

cmd_list_commands :: CommandHandler
cmd_list_commands [] =
    do putStr "= "
       mapM (putStrLn . fst) commandHandlers
       putStrLn ""
       return True

cmd_name :: CommandHandler
cmd_name [] =
    do putStrLn "Kurt"
       putStrLn ""
       return True

cmd_protocol_version :: CommandHandler
cmd_protocol_version _ =
    do putStrLn "2"
       putStrLn ""
       return True

cmd_quit :: CommandHandler
cmd_quit _ =
    do putStrLn "bye!"
       putStrLn ""
       return False

cmd_version :: CommandHandler
cmd_version _ =
    do putStrLn "0.0.1"
       putStrLn ""
       return True

-- TODO

cmd_time_left :: CommandHandler
cmd_time_left [(IntArgument n)] =
    do putStrLn $ "time left: " ++ (show n)
       putStrLn ""
       return True

cmd_clear_board :: CommandHandler
cmd_clear_board _ =
    do putStrLn "clear_board received"
       putStrLn ""
       return True

cmd_komi :: CommandHandler
cmd_komi _ =
    do putStrLn "komi received"
       putStrLn ""
       return True

cmd_play :: CommandHandler
cmd_play _ =
    do putStrLn "play received"
       return True

cmd_genmove :: CommandHandler
cmd_genmove _ =
    do putStrLn "genmove received"
       putStrLn ""
       return True

cmd_boardsize :: CommandHandler
cmd_boardsize [(IntArgument n)] =
    do putStrLn $ "boardsize " ++ (show n) ++ " received"
       putStrLn ""
       return True

cmd_showboard :: CommandHandler
cmd_showboard _ =
    do putStrLn "showboard received"
       putStrLn ""
       return True

