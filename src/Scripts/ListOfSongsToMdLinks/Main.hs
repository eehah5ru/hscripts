{-# LANGUAGE OverloadedStrings #-}
--
--
-- pandoc -f markdown_github -t html di-songs-1.md > di-songs-1.html
-- pandoc -f markdown_github -t html di-songs-2.md > di-songs-2.html
-- cat di-songs-1.html di-songs-2.html > di-songs.html
--
module Scripts.ListOfSongsToMdLinks.Main where

import System.Directory (listDirectory)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import System.IO.Streams.Attoparsec.Text
import Data.Attoparsec.Text
import qualified System.IO.Streams as S
import System.IO.Streams.File (withFileAsInput)
import Data.String.Conversions (cs)
import Data.Text hiding (count, length)
import System.FilePath.Posix (takeFileName)
import GHC.Int (Int64)

import Utils.Streams

import Scripts.ListOfSongsToMdLinks.Types
import Scripts.ListOfSongsToMdLinks.Parser


directory :: FilePath
directory = "/Volumes/qz1_after/tmp/Музыка Дины Жук/"

outFile1 :: FilePath
outFile1 = "/tmp/di-songs-1.md"

outFile2 :: FilePath
outFile2 = "/tmp/di-songs-2.md"

songInfoParser :: Parser (Maybe SongInfo)
songInfoParser = (endOfInput >> pure Nothing) <|> (Just <$> (parseSongInfo)) <|> (many' anyChar >> pure Nothing)

--
--args are:
-- drop
-- take
-- out file
--
convert :: Int64 -> Int64 -> FilePath -> IO ()
convert toDrop toTake outFileName = S.withFileAsOutput outFileName $ \os -> do
  -- isNumbers <- S.fromList [1, 2 ..]
  isFiles <-  S.take toTake =<< S.drop toDrop =<< makeSongFile =<< S.fromList =<< listDirectory directory
  isInfos <- S.take toTake =<< S.drop toDrop =<< parserToInputStream songInfoParser =<< toText =<< toFileName =<< S.fromList =<< listDirectory directory
  isSongFiles <- S.map cs =<< S.map formatMdLink =<< S.map addPrefix =<< S.map fillSongInfo =<< S.zip isFiles isInfos
  osWithLines <- S.intersperse "\n" os
  S.connect isSongFiles osWithLines
  where addPrefix = addFilePrefix "music/"
        toBS (t1, t2) = (cs t1, cs t2)
        mkLinkAndReference (n, sf) = ((formatLink n sf), (formatReference n sf))
        fillSongInfo (sf, si) = sf {info = si}
        toFileName = S.map takeFileName
        makeSongFile = S.map toSongFile

main :: IO ()
main = do
  convert 0 500 outFile1
  convert 500 1000 outFile2


--
--
-- tests
--
--
testFile :: FilePath
testFile = "/Volumes/qz1_after/tmp/Музыка Дины Жук/ABYSS _  eternal soul - SUBARU WRECK.mp3 "

testParseSongInfo :: IO ()
testParseSongInfo = do
  is <- parserToInputStream songInfoParser =<< filePathToText =<< S.fromList [testFile]
  os <- writeToConsole
  S.connect is os
