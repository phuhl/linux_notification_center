{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications (
  startNotificationDaemon
  ) where

import NotificationCenter.Notifications.Notification (
  showNotificationWindow,
  Notification(..)
  )

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  (readTVarIO, modifyTVar, TVar(..), atomically, newTVarIO)

import DBus ( Variant (..) )
import DBus.Client
       ( connectSession, AutoMethod(..), autoMethod, requestName, export
       , nameAllowReplacement, nameReplaceExisting)
import Data.Text ( Text )
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import Data.Map ( Map )

data NotifyState = NotifyState
  { notiStList :: [Notification]
  }


getServerInformation :: IO (Text, Text, Text, Text)
getServerInformation =
  return ("haskell-notification-daemon",
          "abc",
          "0.0.1",
          "1.0")

getCapabilities :: IO [Text]
getCapabilities = return ["body", "body-markup"]

notify :: TVar NotifyState
          -> Text -- ^ Application name
          -> Word32 -- ^ Replaces id
          -> Text -- ^ App icon
          -> Text -- ^ Summary
          -> Text -- ^ Body
          -> [Text] -- ^ Actions
          -> Map Text Variant -- ^ Hints
          -> Int32 -- ^ Expires timeout (milliseconds)
          -> IO Word32
notify istate appName replaceId icon summary body
  actions hints timeout = do
  let newNoti = Notification
        { notiAppName = appName
        , notiId = replaceId
        , notiIcon = icon
        , notiSummary = summary
        , notiBody = body
        , notiActions = actions
        , notiHints = hints
        , notiTimeout = timeout
        }
  atomically $ modifyTVar istate $ \istate' ->
    NotifyState { notiStList = newNoti : notiStList istate' }
  -- new noti-window
  showNotificationWindow newNoti
  -- trigger update in noti-center
  return 0

notificationDaemon :: (AutoMethod f) => f -> IO ()
notificationDaemon onNote = do
    client <- connectSession
    _ <- requestName client "org.freedesktop.Notifications"
         [nameAllowReplacement, nameReplaceExisting]
    export client "/org/freedesktop/Notifications"
      [ autoMethod "org.freedesktop.Notifications"
        "GetServerInformation" getServerInformation
      , autoMethod "org.freedesktop.Notifications"
        "GetCapabilities" getCapabilities
--      , autoMethod "org.freedesktop.Notifications"
--        "CloseNotification" onCloseNote
      , autoMethod "org.freedesktop.Notifications"
        "Notify" onNote
      ]

startNotificationDaemon :: IO ()
startNotificationDaemon = do
  istate <- newTVarIO $ NotifyState []
  forkIO (notificationDaemon (notify istate))
  return ()

