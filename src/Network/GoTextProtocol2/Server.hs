{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Network.GoTextProtocol2.Server
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

this module runs the main gtp handling loop via startLoop.

-}


module Network.GoTextProtocol2.Server (
                                       startLoop
                                      ) where
    

import Data.Char
import Data.List
import System.IO
import System.Random
import Text.Parsec.String (Parser)


import Network.GoTextProtocol2.Server.Parser
import Network.GoTextProtocol2.Server.Types
import Data.Goban.Goban
import Data.Goban (GameState(..), defaultGameState, updateGameState, score, defaultGoban)
-- import Kurt.Move (genMove)
import Kurt.Move (genMove, uctDebug)



type CommandHandler = [Argument] -> GameState -> IO (Either String (String, GameState))

lookupC :: String -> [(String, CommandHandler)] -> Maybe (String, CommandHandler)
lookupC cmd cl = find (\(x, _) -> x == cmd) cl


commandargparserlist :: [([Char], Text.Parsec.String.Parser [Argument])]
commandargparserlist =
    [
     ("boardsize", intArgParser)
    ,("clear_board", noArgumentParser)
    ,("final_score", noArgumentParser)
    ,("genmove", colorArgParser)
    ,("known_command", stringArgParser)
    ,("komi", floatArgParser)
    ,("list_commands", noArgumentParser)
    ,("name", noArgumentParser)
    ,("play", moveArgParser)
    ,("protocol_version", noArgumentParser)
    ,("quit", noArgumentParser)
    ,("showboard", noArgumentParser)
    ,("time_left", timeleftArgParser)
    ,("version", noArgumentParser)
    ,("kurt_simuls", intArgParser)
    ,("gogui-analyze_commands", noArgumentParser)
    ,("kurt_uct_debug", noArgumentParser)
    ]



commandHandlers :: [(String, CommandHandler)]
commandHandlers =
    [
     ("boardsize", cmd_boardsize)
    ,("clear_board", cmd_clear_board)
    ,("final_score", cmd_final_score)
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
    ,("kurt_simuls", cmd_kurt_simuls)
    ,("gogui-analyze_commands", cmd_gogui_analyze_commands)
    ,("kurt_uct_debug", cmd_kurt_uct_debug)
    ]


startLoop :: IO ()
startLoop =
    do
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering
      g <- newStdGen
      loop $ defaultGameState g

loop :: GameState -> IO ()
loop oldState =
    do
      input <- getLine
      parseResult <- return $ pureParseCommand input commandargparserlist
      case parseResult of
        Left err ->
            do
              putStrLn $ "? Couldn't parse command: " ++ (show err)
              putStrLn ""
              loop oldState
        Right (maybeId, Command cmd args) ->
            do
              case lookupC cmd commandHandlers of
                Nothing ->
                    do
                      putStrLn $ "?" ++ (outputIdOrBlank maybeId) ++ "Unrecognized command " ++ cmd
                      newLineFlush
                      loop oldState
                Just (_, handler) ->
                    do
                      result <- handler args oldState
                      case result of
                        Left err ->
                            do
                              putStrLn $ "?" ++ (outputIdOrBlank maybeId) ++ " error: " ++ err
                              newLineFlush
                              loop oldState
                        Right (msg, newState) ->
                            do
                              putStrLn $ "=" ++ (outputIdOrBlank maybeId) ++ msg
                              newLineFlush
                              loop newState

newLineFlush :: IO ()
newLineFlush =
    do
      putStrLn ""


outputIdOrBlank :: Maybe Id -> String
outputIdOrBlank Nothing = " "
outputIdOrBlank (Just lineId) = "[" ++ (show lineId) ++ "] "


cmd_known_command :: CommandHandler
cmd_known_command [(StringArgument cmd)] state =
    return $ case lookupC cmd commandHandlers of
      Nothing -> Right ("false", state)
      Just (_, _) -> Right ("true", state)
cmd_known_command _ _ = error "cmd_known_command called with illegal argument type"

cmd_list_commands :: CommandHandler
cmd_list_commands _ state =
    return $ Right ((reverse $ drop 1 $ reverse $ unlines $ map fst commandHandlers), state)

cmd_name :: CommandHandler
cmd_name _ state =
    return $ Right ("kurt", state)

cmd_protocol_version :: CommandHandler
cmd_protocol_version _ state =
    return $ Right ("2", state)

cmd_quit :: CommandHandler
cmd_quit _ _ =
    error "bye!"

cmd_version :: CommandHandler
cmd_version _ state =
    return $ Right ("0.0.1", state)


cmd_clear_board :: CommandHandler
cmd_clear_board [] state =
    return $ Right ("", state {
                  goban = clearGoban (goban state)
                 ,moveHistory = []
                 ,koBlocked = []
                 ,blackPrisoners = 0
                 ,whitePrisoners = 0
                 ,ourRandomGen = g
                 })
    where
      (g, _g) = split (ourRandomGen state)
cmd_clear_board _ _ = error "cmd_clear_board called with illegal argument type"

cmd_komi :: CommandHandler
cmd_komi [(FloatArgument f)] state =
    return $ Right ("", state { komi = f })
cmd_komi _ _ = error "cmd_komi called with illegal argument type"

cmd_boardsize :: CommandHandler
cmd_boardsize [(IntArgument n)] state =
    return $ Right ("", state { goban = (defaultGoban n) })
cmd_boardsize _ _ = error "cmd_boardsize called with illegal argument type"

cmd_showboard :: CommandHandler
cmd_showboard [] state =
    return $ Right ("showboard received:\n" ++ (showboard (goban state)), state)
cmd_showboard _ _ = error "cmd_showboard called with illegal argument type"

cmd_play :: CommandHandler
cmd_play [(MoveArgument move)] state =
    return $ Right ("", updateGameState state move)
cmd_play _ _ = error "cmd_play called with illegal argument type"

cmd_genmove :: CommandHandler
cmd_genmove [(ColorArgument color)] state = do
  move <- genMove state color
  return $ Right (show move, updateGameState state move)

cmd_genmove _ _ = error "cmd_genmove called with illegal argument type"

cmd_final_score :: CommandHandler
cmd_final_score [] state =
    return $ Right (scoreString scoreFloat, state)
    where
      scoreString s
          | s < 0 = "W+" ++ (show (-1 * s))
          | s > 0 = "B+" ++ (show s)
          | otherwise = "0"
      scoreFloat = (score state)
cmd_final_score _ _ = error "cmd_final_score called with illegal argument type"


cmd_kurt_simuls :: CommandHandler
cmd_kurt_simuls [(IntArgument n)] state =
    return $ Right ("", state { simulCount = n })
cmd_kurt_simuls _ _ = error "cmd_kurt_simuls called with illegal argument type"


cmd_gogui_analyze_commands :: CommandHandler
cmd_gogui_analyze_commands [] state =
    return $ Right (
           "gfx/kurt_uct_debug/kurt_uct_debug"
          , state)
cmd_gogui_analyze_commands _ _ = error "cmd_gogui_analyze_commands called with illegal argument type"

cmd_kurt_uct_debug :: CommandHandler
cmd_kurt_uct_debug [] state =
    return $ Right (gfxString, state)
    where
      gfxString = uctDebug state (ourRandomGen state)
cmd_kurt_uct_debug _ _ = error "cmd_kurt_uct_debug called with illegal argument type"


cmd_time_left :: CommandHandler
cmd_time_left [(TimeLeftArgument (seconds, stones))] state =
    return $ Right ("time left: " ++ (show (seconds, stones, milliseconds)),
           state { timePerMove = milliseconds } )
    where
      milliseconds = (seconds * 900) `div` stones

cmd_time_left _ _ = error "cmd_time_left called with illegal argument type"

