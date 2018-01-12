{-# LANGUAGE OverloadedStrings #-}

module Utils.Streams where
import qualified Data.String.Conversions as SC
import qualified System.IO.Streams as S
import Data.Text (Text, pack)

toText :: (SC.ConvertibleStrings a b) => S.InputStream a -> IO (S.InputStream b)
toText = S.map SC.cs

filePathToText :: S.InputStream FilePath -> IO (S.InputStream Text)
filePathToText = S.map pack

writeToConsole :: (Show a) => IO (S.OutputStream a)
writeToConsole = S.makeOutputStream (maybe (return ()) (putStrLn . show))
