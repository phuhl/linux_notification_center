module NotificationCenter.Notifications.Data
  ( Urgency(..)
  , CloseType (..)
  , Notification(..)
  ) where

import Data.Text as Text
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import qualified Data.Map as Map ( Map )
import Data.List ( sortOn )
import DBus ( Variant (..), Signal )


data Urgency = Normal | Low | High deriving Eq
data CloseType = Timeout | User | CloseByCall | Other deriving Eq

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
  , notiSendClosedMsg :: Bool -- ^ If notiOnClosed should be ignored
  , notiOnClosed :: CloseType -> IO ()
    -- ^ Should be called when the notification is closed, either by
    --   timeout or by user
  , notiOnAction :: String -> IO ()
    -- ^ Should be called when an action is used
  }

