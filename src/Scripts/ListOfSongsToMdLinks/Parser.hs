{-# LANGUAGE OverloadedStrings #-}

module Scripts.ListOfSongsToMdLinks.Parser where

import Prelude hiding (takeWhile)

import Data.Char (isDigit)
import Data.Text hiding (takeWhile, count)
import Data.Attoparsec.Text
import Control.Applicative

import Scripts.ListOfSongsToMdLinks.Types

parseSongInfo :: Parser (SongInfo)
parseSongInfo = do
  author <- manyTill anyChar (string " - ")
  name <- manyTill anyChar (string ".mp3")
  return $ SongInfo (pack author) (pack name)
