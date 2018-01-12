{-# LANGUAGE OverloadedStrings #-}

module Scripts.ListOfSongsToMdLinks.Types where

import Prelude hiding (concat)

import Data.Text
import qualified Data.Text as T
import System.FilePath.Posix ((</>))

data SongInfo = SongInfo { author :: Text
                         , name :: Text }
              | EmptySongInfo deriving (Show)

emptySongInfo = EmptySongInfo

data SongFile = SongFile { info :: SongInfo
                         , file :: FilePath } deriving (Show)

toSongFile :: FilePath -> SongFile
toSongFile = SongFile emptySongInfo

formatLink :: Int -> SongFile -> Text
formatLink n (SongFile EmptySongInfo _) = "[empty link]"
                                          `T.append` "["
                                          `T.append` (pack . show $ n)
                                          `T.append` "]"
formatLink n (SongFile si f) = "["
                               `T.append` (name si)
                               `T.append` "]["
                               `T.append` (pack . show $ n)
                               `T.append` "]"

formatReference :: Int -> SongFile -> Text
formatReference n (SongFile EmptySongInfo _) = "["
                                               `T.append` (pack . show $ n)
                                               `T.append` "]: "
                                               `T.append` "empty reference"
formatReference n (SongFile si f) = "["
                                    `T.append` (pack . show $ n)
                                    `T.append` "]: "
                                    `T.append` (pack $ f)

addFilePrefix :: FilePath -> SongFile -> SongFile
addFilePrefix fp sf = sf {file = fp </> (file sf)}

formatMdLink :: SongFile -> Text
formatMdLink (SongFile EmptySongInfo f) = "[]("
                                          `T.append` (pack f)
                                          `T.append` ")"
formatMdLink (SongFile si f) = "["
                               `T.append` (name si)
                               `T.append` "]("
                               `T.append` (pack f)
                               `T.append` ")"
