{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Prelude hiding (takeWhile)

import Data.Char (isDigit)
import Data.Text hiding (takeWhile, count)
import Data.Attoparsec.Text
import Control.Applicative

import Types

-- parseEndOfLine :: Parser a
-- parseEndOfLine = many $ inClass "\n\r"

parseTime :: Parser Time
parseTime = do
  hours <- parseInt
  char ':'
  mins <- parseInt
  char ':'
  secs <- parseInt
  char '.'
  msecs <- parseInt >>= return . (* 10)
  return $ Time hours mins secs msecs
  where parseInt = takeWhile isDigit >>= return . (read :: String -> Int) . unpack

parseSubtitle :: Parser Subtitle
parseSubtitle = doParseSubtitle
  where doParseSubtitle = do
         string "Dialogue: 0"
         char ','
         startTime <- parseTime
         char ','
         endTime <- parseTime
         char ','
         string "Default,,0000,0000,0000,,"
         msg <- takeTill $ inClass "\r\n"
         return $ Subtitle startTime endTime msg

emptySpace :: Parser ()
emptySpace = do
  skipSpace
  skipWhile endOfLine
  skipSpace
  where endOfLine = inClass "\r\n"

parseCollision :: Parser Collision
parseCollision = do
  try emptySpace
  before <- parseSubtitle
  emptySpace
  problematic <- parseSubtitle
  emptySpace
  after <- parseSubtitle
  return $ Collision before problematic after
  where endOfLine = inClass "\r\n"
        skipBetween = do
          skipSpace
          skipWhile endOfLine
          skipSpace



parseCollisionsFile :: Parser [Collision]
parseCollisionsFile = do
  cs <- parseCollision `sepBy'` (many' (endOfLine))
  return cs
