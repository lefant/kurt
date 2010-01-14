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

This is mostly a cleaned up copy of Network.FTP.Server.Parser by John
Goerzen.

-}

module Network.GoTextProtocol2.Server.Parser (
                                              parseCommand
                                             ) where

import Text.ParserCombinators.Parsec
import Data.String.Utils
import System.IO(Handle,hGetLine)
import Data.Char

alpha :: forall st. GenParser Char st Char
alpha = oneOf (['A'..'Z'] ++ ['a'..'z']) <?> "alphabetic character"

word :: forall st. GenParser Char st [Char]
word = many1 alpha

args :: Parser String
args = try (do char ' '
               r <- many anyChar
               eof 
               return r)
       <|> return ""
       

command :: Parser (String, String)
command = do
          x <- word
          y <- args
          eof
          return (map toUpper x, y)


parseCommand :: Handle -> IO (Either ParseError (String, String))
parseCommand h =
    do input <- hGetLine h
       return $ parse command "(unknown)" (rstrip input)
