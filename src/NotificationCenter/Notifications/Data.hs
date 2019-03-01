module NotificationCenter.Notifications.Data
  ( Urgency(..)
  , Config (..)
  , Notification(..)
  ) where

import Data.Text as Text
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import qualified Data.Map as Map ( Map )
import Data.List ( sortOn )
import DBus ( Variant (..) )


data Urgency = Normal | Low | High deriving Eq

data Notification = Notification
  { notiAppName :: Text -- ^ Application name
  , notiRepId :: Word32 -- ^ Replaces id
  , notiId :: Int -- ^ Id
  , notiIcon :: Text -- ^ App icon
  , notiSummary :: Text -- ^ Summary
  , notiBody :: Text -- ^ Body
  , notiActions :: [Text] -- ^ Actions
  , notiHints :: Map.Map Text Variant -- ^ Hints
  , notiUrgency :: Urgency
  , notiTimeout :: Int32 -- ^ Expires timeout (milliseconds)
  , notiTime :: Text
  , notiTransient :: Bool
  }

data Config = Config
  {
  -- notification-center
    configBarHeight :: Int
  , configWidth :: Int
  , configStartupCommand :: String
  , configNotiCenterMonitor :: Int
  , configNotiCenterNewFirst :: Bool
  , configIgnoreTransient :: Bool
  , configMatchingRules :: [((Notification -> Bool), (Notification -> Notification), Maybe String)]

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
  }
