module NotificationCenter.Notifications.Data
  ( Urgency(..)
  , Config (..)
  ) where

data Urgency = Normal | Low | High deriving Eq

data Config = Config
  {
    configBarHeight :: Int
  , configNotiDefaultTimeout :: Int
  , configDistanceTop :: Int
  , configDistanceBetween :: Int
  } deriving Show

