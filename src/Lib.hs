{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import System.IO.Streams.Attoparsec.Text
import Data.Attoparsec.Text
import qualified System.IO.Streams as S
import System.IO.Streams.File (withFileAsInput)
import Data.String.Conversions (cs)
import Data.Text hiding (count, length)

import Parser
import Types

testSubtitle :: Text
testSubtitle = "Dialogue: 0,0:16:29.82,0:16:35.71,Default,,0000,0000,0000,,I am calm and rested."

testCollision :: Text
testCollision = "Dialogue: 0,0:16:29.82,0:16:35.71,Default,,0000,0000,0000,,I am calm and rested.\n\
\ Dialogue: 0,0:16:35.81,0:16:40.71,Default,,0000,0000,0000,,Calm and rested.\n\
\ Dialogue: 0,0:16:40.81,0:16:47.71,Default,,0000,0000,0000,,I feel at home \\Nhere under this highway.\n"

testCollisionsFileName :: FilePath
testCollisionsFileName = "/Volumes/qz1_now/Users/eehah5ru/it/utils/subtitles-checker/data/collisions.txt"

testParseSubtitle :: IO ()
testParseSubtitle = do
  is <- S.fromList [testSubtitle]
  parseFromStream parseSubtitle is >>= putStrLn . show
  S.read is >>= putStrLn . show

testParseCollision :: IO ()
testParseCollision = do
  is <- S.fromList [testCollision]
  parseFromStream parseCollision is >>= putStrLn . show
  S.read is >>= putStrLn . show


testParseCollisionsFile :: IO ()
testParseCollisionsFile = do
  withFileAsInput testCollisionsFileName (toText >=> f)
  where toText = S.map cs
        f is = do
          parseFromStream parseCollisionsFile is >>= putStrLn . show . length
          S.read is >>= putStrLn . show


-- main :: IO ()
-- main = do
--   withFileAsInput testCollisionsFileName (toText >=> parseCollisions >=> calculateDistances >=> S.toList) >>= putStrLn . show
--   where toText = S.map cs
--         calculateDistances is = S.map fromCollision is
--         printDistances is = S.toList is >>= putStrLn . show
--         collisionsFileParser = (endOfInput >> pure Nothing) <|> (Just <$> parseCollision)


main2 :: IO ()
main2 = withFileAsInput testCollisionsFileName f
  where
        printCollision = S.makeOutputStream (maybe (return ()) (putStrLn . show))
        toText = S.map cs
        parseCollisions is = parserToInputStream collisionsFileParser is
        collisionsFileParser = (endOfInput >> pure Nothing) <|> (Just <$> (parseCollision)) <|> (emptySpace >> pure Nothing)
        f is = do
          is2 <- toText is >>= parseCollisions >>= S.map fromCollision
          os <- printCollision
          S.connect is2 os
