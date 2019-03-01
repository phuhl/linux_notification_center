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
  , configStartupCommand :: String
  , configNotiCenterMonitor :: Int
  , configNotiCenterNewFirst :: Bool
  , configIgnoreTransient :: Bool

  -- notification-center-notification-popup
  , configNotiDefaultTimeout :: Int
  , configDistanceTop :: Int
  , configDistanceRight :: Int
  , configDistanceBetween :: Int
  , configWidthNoti :: Int
  , configNotiMonitor :: Int

  -- buttons
  , configButtonsPerRow :: Int
  , configButtonHeight :: Int
  , configButtonMargin :: Int
  , configLabels :: String
  , configCommands :: String
  , configUserButtonColor :: String
  , configUserButtonHover :: String
  , configUserButtonHoverColor :: String
  , configUserButtonBackground :: String
  , configUserButtonTextSize :: String
  , configUserButtonState1 :: String
  , configUserButtonState2 :: String
  , configUserButtonState1Color :: String
  , configUserButtonState2Color :: String
  , configUserButtonState1Hover :: String
  , configUserButtonState2Hover :: String
  , configUserButtonState1HoverColor :: String
  , configUserButtonState2HoverColor :: String

  -- colors
  , configBackground :: String
  , configBackgroundNoti :: String
  , configNotiLabelColor :: String
  , configCritical :: String
  , configCriticalInCenter :: String
  , configButtonColor :: String
  , configButtonHover :: String
  , configButtonHoverColor :: String
  , configButtonBackground :: String
  , configLabelColor :: String
  , configCriticalColor :: String
  , configCriticalInCenterColor :: String
  } deriving Show

