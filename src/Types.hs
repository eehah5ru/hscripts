{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T

--
--
-- Time
--
--
data Time = Time { hours :: Int
                 , mins :: Int
                 , secs :: Int
                 , millis :: Int} deriving (Show)

emptyTime :: Time
emptyTime = Time 0 0 0 0

toInt (Time h m s ms) = let hourMullis = h * 3600 * 1000
                            minuteMillis = m * 60 * 1000
                            secondsMillis = s * 1000
                        in ms + secondsMillis + minuteMillis + hourMullis
--
--
-- Subtitle
--
--
data Subtitle = Subtitle { start :: Time
                         , end :: Time
                         , msg :: T.Text } deriving (Show)

emptySubtitle :: Subtitle
emptySubtitle = Subtitle emptyTime emptyTime T.empty


data Collision = Collision { before :: Subtitle
                           , problematic :: Subtitle
                           , after :: Subtitle } deriving (Show)


data Distance = Distance Int Int deriving (Show)

fromCollision :: Collision -> Distance
fromCollision (Collision b p a) = Distance (timeBetween b p) (timeBetween p a)
  where timeBetween (Subtitle _ t1 _) (Subtitle t2 _ _) = (toInt t2) - (toInt t1)
