{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter.Notifications.Notification (
  showNotificationWindow,
  Notification(..)
  ) where

import TransparentWindow
import NotificationCenter.Notifications.Notification.Glade (glade)

import Data.Text as Text
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import Data.Map ( Map )

import GI.Gtk (widgetShowAll, windowMove)

import DBus ( Variant (..) )
data Notification = Notification
  { notiAppName :: Text -- ^ Application name
  , notiId:: Word32 -- ^ Replaces id
  , notiIcon:: Text -- ^ App icon
  , notiSummary:: Text -- ^ Summary
  , notiBody:: Text -- ^ Body
  , notiActions:: [Text] -- ^ Actions
  , notiHints:: Map Text Variant -- ^ Hints
  , notiTimeout:: Int32 -- ^ Expires timeout (milliseconds)
  }


showNotificationWindow :: Notification -> IO ()
showNotificationWindow noti = do

--  startSetTimeThread objs
  objs <- createTransparentWindow (Text.pack glade)
    ["main_window"] Nothing

  mainWindow <- window objs "main_window"

  (screenH, screenW) <- getScreenProportions mainWindow
  windowMove mainWindow (screenW - 500) 100

  startTimeoutThread objs $ notiTimeout noti

  widgetShowAll mainWindow

  return ()


startTimeoutThread objs timeout =
  return ()
