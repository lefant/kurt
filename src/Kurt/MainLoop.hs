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


module Kurt.MainLoop ( startLoop
                     ) where
    

import Control.Arrow (second)
import Control.Monad.ST (stToIO, RealWorld)
import System.IO
import Text.Parsec.String (Parser)
import Data.Char
import Data.List
import qualified Data.Map as M (assocs)
import Data.Tree (rootLabel, subForest)
import Data.Tree.Zipper (tree)
import Text.Printf (printf)


import Network.GoTextProtocol2.Server.Parser
import Network.GoTextProtocol2.Types
import Data.Goban.GameState (GameState(..), newGameState, showGameState, updateGameState, scoreGameState, makeStonesAndLibertyHeuristic, nextMoves, nextMoveColor)
import Data.Goban.Types (gtpShowMove, gtpShowVertex, Move(..), Stone(..), Color(..))
import Data.Goban.STVectorGoban (allStones)

import Kurt.GoEngine (EngineState(..), newEngineState, genMove)
import Data.Tree.UCT.GameTree (MoveNode(..))


-- import Debug.TraceOrId (trace)


type CommandHandler s = [Argument] -> EngineState s -> IO (Either String (String, EngineState s))


lookupC :: String -> [(String, CommandHandler RealWorld)] -> Maybe (String, CommandHandler RealWorld)
lookupC cmd = find (\(x, _) -> x == cmd)


commandargparserlist :: [(String, Text.Parsec.String.Parser [Argument])]
commandargparserlist =
    [
     ("boardsize", intArgParser)
    ,("clear_board", noArgumentParser)
    ,("final_score", noArgumentParser)
    ,("final_status_list", stringArgParser)
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

    ,("gogui-analyze_commands", noArgumentParser)

    ,("kurt_configure", maybeKeyValueArgParser)

    ,("kurt_uct_tree", noArgumentParser)
    ,("kurt_ravemap", noArgumentParser)
    ,("kurt_heuristic_total", noArgumentParser)
    ,("kurt_heuristic_stone", noArgumentParser)
    ,("kurt_heuristic_liberty_min", noArgumentParser)
    ,("kurt_heuristic_liberty_avg", noArgumentParser)
    ,("kurt_heuristic_center", noArgumentParser)
    ]



commandHandlers :: [(String, CommandHandler RealWorld)]
commandHandlers =
    [
     ("boardsize", cmd_boardsize)
    ,("clear_board", cmd_clear_board)
    ,("final_score", cmd_final_score)
    ,("final_status_list", cmd_final_status_list)
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

    ,("gogui-analyze_commands", cmd_gogui_analyze_commands)

    ,("kurt_configure", cmd_kurt_configure)

    ,("kurt_heuristic_total", cmd_kurt_heuristic_total)
    ,("kurt_heuristic_stone", make_cmd_kurt_heuristic (1,0,0,0))
    ,("kurt_heuristic_liberty_min", make_cmd_kurt_heuristic (0,1,0,0))
    ,("kurt_heuristic_liberty_avg", make_cmd_kurt_heuristic (0,0,1,0))
    ,("kurt_heuristic_center", make_cmd_kurt_heuristic (0,0,0,1))
    ,("kurt_uct_tree", cmd_kurt_uct_tree)
    ,("kurt_ravemap", cmd_kurt_ravemap)
    ]


startLoop :: IO ()
startLoop =
    do
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering
      stToIO newEngineState >>= loop

loop :: EngineState RealWorld -> IO ()
loop oldState =
    do
      input <- getLine
      let parseResult = pureParseCommand input commandargparserlist
      case parseResult of
        Left err ->
            do
              putStrLn $ "? Couldn't parse command: " ++ show err
              putStrLn ""
              loop oldState
        Right (maybeId, Command cmd args) ->
              case lookupC cmd commandHandlers of
                Nothing ->
                    do
                      putStrLn $ "?" ++ outputIdOrBlank maybeId ++ "Unrecognized command " ++ cmd
                      newLineFlush
                      loop oldState
                Just (_, handler) ->
                    do
                      result <- handler args oldState
                      case result of
                        Left err ->
                            do
                              putStrLn $ "?" ++ outputIdOrBlank maybeId ++ " error: " ++ err
                              newLineFlush
                              loop oldState
                        Right (msg, newState) ->
                            do
                              putStrLn $ "=" ++ outputIdOrBlank maybeId ++ msg
                              newLineFlush
                              loop newState

newLineFlush :: IO ()
newLineFlush =
    putStrLn ""


outputIdOrBlank :: Maybe Id -> String
outputIdOrBlank Nothing = " "
outputIdOrBlank (Just lineId) = show lineId ++ " "


cmd_known_command :: CommandHandler RealWorld
cmd_known_command [StringArgument cmd] state =
    return $ case lookupC cmd commandHandlers of
      Nothing -> Right ("false", state)
      Just (_, _) -> Right ("true", state)
cmd_known_command _ _ = error "cmd_known_command called with illegal argument type"

cmd_list_commands :: CommandHandler RealWorld
cmd_list_commands _ state =
    return $ Right (reverse $ drop 1 $ reverse $ unlines $ map fst commandHandlers, state)

cmd_name :: CommandHandler RealWorld
cmd_name _ state =
    return $ Right ("kurt", state)

cmd_protocol_version :: CommandHandler RealWorld
cmd_protocol_version _ state =
    return $ Right ("2", state)

cmd_quit :: CommandHandler RealWorld
cmd_quit _ _ =
    error "bye!"

cmd_version :: CommandHandler RealWorld
cmd_version _ state =
    return $ Right ("0.0.1", state)


cmd_clear_board :: CommandHandler RealWorld
cmd_clear_board [] state = do
  gState' <- stToIO $ newGameState (boardSize state) (getKomi state)
  return $ Right ("", state { getGameState = gState' })
cmd_clear_board _ _ = error "cmd_clear_board called with illegal argument type"

cmd_komi :: CommandHandler RealWorld
cmd_komi [FloatArgument f] state =
    return $ Right ("",
                    state {
                      getKomi = f
                    , getGameState = gState { komi = f } } )
    where
      gState = getGameState state
cmd_komi _ _ = error "cmd_komi called with illegal argument type"

cmd_boardsize :: CommandHandler RealWorld
cmd_boardsize [IntArgument n] state =
    return $ Right ("",
                    state {
                      getGameState =
                      error "gamestate undefined after boardsize until clear board received"
                    , boardSize = n } )
cmd_boardsize _ _ = error "cmd_boardsize called with illegal argument type"

cmd_showboard :: CommandHandler RealWorld
cmd_showboard [] state = do
  str <- stToIO $ showGameState $ getGameState state
  return $ Right ("showboard\n" ++ str, state)
cmd_showboard _ _ = error "cmd_showboard called with illegal argument type"

cmd_play :: CommandHandler RealWorld
cmd_play [MoveArgument move] state = do
  gState' <- stToIO $ updateGameState (getGameState state) move
  -- str <- stToIO $ showGameState gState'
  -- trace ("cmd_play board: " ++ str) $ return ()
  return $ Right ("", state { getGameState = gState' })
cmd_play _ _ = error "cmd_play called with illegal argument type"

cmd_genmove :: CommandHandler RealWorld
cmd_genmove [ColorArgument color] state = do
  (move, state') <- genMove state color
  gState' <- stToIO $ updateGameState (getGameState state) move
  -- str <- stToIO $ showGameState gState'
  -- trace ("cmd_genmove board:" ++ str) $ return ()
  return $ Right (gtpShowMove move, state' { getGameState = gState' })
cmd_genmove _ _ = error "cmd_genmove called with illegal argument type"

cmd_final_score :: CommandHandler RealWorld
cmd_final_score [] state = do
  scoreFloat <- stToIO $ scoreGameState $ getGameState state
  return $ Right (scoreString scoreFloat, state)
    where
      scoreString s
          | s < 0 = "W+" ++ show (-1 * s)
          | s > 0 = "B+" ++ show s
          | otherwise = "0"
cmd_final_score _ _ = error "cmd_final_score called with illegal argument type"

cmd_final_status_list :: CommandHandler RealWorld
cmd_final_status_list [StringArgument arg] state =
    case arg of
      "dead" -> return $ Right ("", state)
      "seki" -> return $ Right ("", state)
      "alive" ->
          do
            stones <- stToIO $ allStones $ goban $ getGameState state
            return $ Right (unwords $ map gtpShowVertex stones, state)
      other -> error ("cmd_final_status_list illegal arg: " ++ other)
cmd_final_status_list _ _ = error "cmd_final_status_list called with illegal argument type"


cmd_time_left :: CommandHandler RealWorld
cmd_time_left [TimeLeftArgument seconds stones] state =
    return
    $ Right ("", state { timePerMove = milliseconds } )
    where
      milliseconds =
          if stones == 0
          then (seconds * 900) `div` estMaxMoves
          else (seconds * 900) `div` stones
      estMaxMoves = (boardSize state + 1) ^ (2 :: Int)
cmd_time_left _ _ = error "cmd_time_left called with illegal argument type"




-- gogui analyze gtp extensions
-------------------------------

cmd_gogui_analyze_commands :: CommandHandler RealWorld
cmd_gogui_analyze_commands [] state =
    return $ Right (
           "param/kurt_configure/kurt_configure\n"
           ++ "gfx/kurt_uct_tree/kurt_uct_tree\n"
           ++ "gfx/kurt_ravemap/kurt_ravemap\n"
           ++ "gfx/kurt_heuristic_total/kurt_heuristic_total\n"
           ++ "gfx/kurt_heuristic_stone/kurt_heuristic_stone\n"
           ++ "gfx/kurt_heuristic_liberty_min/kurt_heuristic_liberty_min\n"
           ++ "gfx/kurt_heuristic_liberty_avg/kurt_heuristic_liberty_avg\n"
           ++ "gfx/kurt_heuristic_center/kurt_heuristic_center"
          , state)
cmd_gogui_analyze_commands _ _ = error "cmd_gogui_analyze_commands called with illegal argument type"


cmd_kurt_heuristic_total :: CommandHandler RealWorld
cmd_kurt_heuristic_total [] state =
  make_cmd_kurt_heuristic (getHeuWeights state) [] state
cmd_kurt_heuristic_total _ _ = error "cmd_kurt_heuristic_total called with illegal argument type"

make_cmd_kurt_heuristic :: (Int, Int, Int, Int) -> CommandHandler RealWorld
make_cmd_kurt_heuristic hWeights [] state = do
  moves <- stToIO $ nextMoves gState color
  slHeu <- stToIO $ makeStonesAndLibertyHeuristic gState hWeights
  let str = concatMap
            (\move@(Move (Stone p _c))
                 -> " " ++ gtpShowVertex p
                    ++ printf " %.2f" (((fst $ slHeu move) - 0.5) * 2 * flipSig))
                    -- ++ printf " %.2f" (fst $ slHeu move))
            moves
  return $ Right ("INFLUENCE" ++ str, state)
    where
      flipSig = if color == Black then 1 else -1
      color = nextMoveColor gState
      gState = getGameState state
make_cmd_kurt_heuristic _ _ _ = error "make_cmd_kurt_heuristic called with illegal argument type"

cmd_kurt_uct_tree :: CommandHandler RealWorld
cmd_kurt_uct_tree [] state = do
  let str = concatMap
            (\(Move (Stone p _c), v)
                 -> " " ++ gtpShowVertex p
                    ++ printf " %.2f" ((v - 0.5) * 2 * flipSig))
                    -- ++ printf " %.2f" (fst $ slHeu move))
            ucts
  return $ Right ("INFLUENCE" ++ str, state)
    where
      ucts = map (\l -> (nodeMove l, nodeValue l))
                     $ map rootLabel $ subForest $ tree $ getUctTree state
      flipSig = if color == Black then 1 else -1
      color = nextMoveColor $ getGameState state

cmd_kurt_uct_tree _ _ = error "cmd_kurt_uct_tree called with illegal argument type"

cmd_kurt_ravemap :: CommandHandler RealWorld
cmd_kurt_ravemap [] state = do
  let str = concatMap
            (\(Move (Stone p _c), v)
                 -> " " ++ gtpShowVertex p
                    ++ printf " %.2f" ((v - 0.5) * 2 * flipSig))
                    -- ++ printf " %.2f" (fst $ slHeu move))
            raves
  return $ Right ("INFLUENCE" ++ str, state)
    where
      raves = map (second fst) $ M.assocs $ getRaveMap state

      flipSig = if color == Black then 1 else -1
      color = nextMoveColor $ getGameState state

cmd_kurt_ravemap _ _ = error "cmd_kurt_ravemap called with illegal argument type"





-- purely kurt specific gtp extensions
--------------------------------------

cmd_kurt_configure :: CommandHandler RealWorld
cmd_kurt_configure [MaybeKeyValueArgument Nothing] state =
    return $ Right (
                    unlines $ [
                     "maxruns " ++ show (maxRuns state)
                    ,"maxtime " ++ show (timePerMove state)
                    ]
          , state)
cmd_kurt_configure [MaybeKeyValueArgument (Just (str, n))] state =
    return $ case str of
               "maxruns" -> Right ("maxRuns set to " ++ show n, state { maxRuns = n })
               "maxtime" -> Right ("timePerMove set to " ++ show n, state { timePerMove = n })
               other -> Left ("unknown configuration key " ++ show other)
cmd_kurt_configure _ _ = error "cmd_kurt_configure called with illegal argument type"
