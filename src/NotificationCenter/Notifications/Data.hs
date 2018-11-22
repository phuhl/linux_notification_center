module NotificationCenter.Notifications.Data
  ( Urgency(..)
  , Config (..)
  ) where

data Urgency = Normal | Low | High deriving Eq

data Config = Config
  {
    configBarHeight :: Int
  , configWidth :: Int

  , configNotiDefaultTimeout :: Int
  , configDistanceTop :: Int
  , configDistanceRight :: Int
  , configDistanceBetween :: Int
  , configWidthNoti :: Int

  -- colors
  , configBackground :: String
  , configCritical :: String
  , configCriticalInCenter :: String
  , configButtonColor :: String
  , configButtonHover :: String
  , configButtonBackground :: String
  , configLabelColor :: String
  , configCriticalColor :: String
  , configCriticalInCenterColor :: String
  } deriving Show

