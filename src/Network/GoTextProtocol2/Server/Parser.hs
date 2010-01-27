{-# OPTIONS -O2 -Wall -Werror -Wwarn -XRankNTypes #-}

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
   Module     : Network.GoTextProtocol2.Server.Parser
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Parsing GTP protocol messages

-}

module Network.GoTextProtocol2.Server.Parser (
                                              pureParseCommand
                                             ,noArgumentParser
                                             ,intArgParser
                                             ,floatArgParser
                                             ,colorArgParser
                                             ,moveArgParser
                                             ,stringArgParser
                                             ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Char (toLower)
import Monad (liftM)

import Network.GoTextProtocol2.Server.Types
import Data.Goban.Utils (
                         Move(..)
                        ,Stone(..)
                        ,Color(..)
                        ,letterToX
                        )


type CommandArgParserList = [(String, Parser [Argument])]


pureParseCommand :: String -> CommandArgParserList -> Either ParseError (Maybe Id, Command)
pureParseCommand input commandargparserlist =
    parse (line commandargparserlist) "(unknown)" $ map toLower input


line :: CommandArgParserList -> Parser (Maybe Id, Command)
line commandList =
    do
      {- FIXME: more work here
      skipMany (do
                 emptyline
             <|> commentline)
       -}
      (idCommandline commandList)
  <|> (commandline commandList)
  <?> "command"


{-
-- FIXME: not yet

emptyline :: Parser ()
emptyline = spaces

commentline :: Parser ()
commentline =
    do
      char '#'
      return ()
      -- FIXME: need to do some more work here
-}

commandline :: CommandArgParserList -> Parser (Maybe Id, Command)
commandline commandList =
    do
      c <- (command commandList)
      spaces
      return (Nothing, c)


idCommandline :: CommandArgParserList -> Parser (Maybe Id, Command)
idCommandline commandList =
    do
      lineId <- commandId
      space
      spaces
      c <- (command commandList)
      spaces
      return (Just lineId, c)

commandId :: Parser Int
commandId =
    do
      char '['
      i <- parseInt
      char ']'
      return i





command :: CommandArgParserList -> Parser Command
command commandList =
    choice $ map command2parser commandList



command2parser :: (String, Parser [Argument]) -> Parser Command
command2parser (str, argParser) =
    do
      cmd <- try (string str)
      args <- argParser
      return (Command cmd args)






noArgumentParser :: Parser [Argument]
noArgumentParser = return []


stringArgParser :: Parser [Argument]
stringArgParser =
    do
      space
      spaces
      str <- many1 (letter <|> char '-' <|> char '_')
      return $ [StringArgument (map toLower str)]


floatArgParser :: Parser [Argument]
floatArgParser =
    do
      space
      spaces
      (do
        (try
         (do
           d1 <- many1 digit
           char '.'
           d2 <- many1 digit
           n <- return $ read (d1 ++ ['.'] ++ d2)
           return $ [FloatArgument n]))
        <|> (do
              d1 <- many1 digit
              n <- return $ read (d1 ++ ".0")
              return $ [FloatArgument n])
        <?> "komi value (ie. something like 6.5 or 0)")


moveArgParser :: Parser [Argument]
moveArgParser =
    do
      (ColorArgument c) <- colorParser
      space
      spaces
      (do
        (do
          try (string "pass")
          return [MoveArgument (Pass c)])
         <|> (do
               l <- letter
               n <- parseInt
               return [MoveArgument $ StoneMove $ Stone ((letterToX l, n), c)])
         <?> "vertex (ie. something like A1, H8, Z25 or pass)")



colorArgParser :: Parser [Argument]
colorArgParser =
    do
      c <- colorParser
      return [c]

colorParser :: Parser Argument
colorParser =
    do
      space
      spaces
      c <- (do
             try (string "white")
         <|> try (string "w")
         <|> try (string "black")
         <|> try (string "b")
         <?> "string describing color (ie. white, black, w, W, b or B)")
      case c of
        "white" -> return $ ColorArgument White
        "w" -> return $ ColorArgument White
        "W" -> return $ ColorArgument White
        "black" -> return $ ColorArgument Black
        "b" -> return $ ColorArgument Black
        "B" -> return $ ColorArgument Black
        str -> error $ "colorArgParser: unexpected " ++ str


intArgParser :: Parser [Argument]
intArgParser =
    do
      space
      spaces
      n <- parseInt
      return $ [IntArgument n]

parseInt :: Parser Int
parseInt = liftM read $ many1 digit
