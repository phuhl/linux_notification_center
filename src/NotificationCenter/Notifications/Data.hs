module NotificationCenter.Notifications.Data
  ( Urgency(..)
  , Config (..)
  ) where

data Urgency = Normal | Low | High deriving Eq

data Config = Config
  {
  -- notification-center
    configBarHeight :: Int
  , configWidth :: Int

  -- notification-center-notification-popup
  , configNotiDefaultTimeout :: Int
  , configDistanceTop :: Int
  , configDistanceRight :: Int
  , configDistanceBetween :: Int
  , configWidthNoti :: Int

  -- buttons
  , configButtonsPerRow :: Int
  , configButtonHeight :: Int
  , configButtonMargin :: Int
  , configLabels :: String
  , configCommands :: String
  , configUserButtonColor :: String
  , configUserButtonHover :: String
  , configUserButtonBackground :: String
  , configUserButtonTextSize :: String

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

