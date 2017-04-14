{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications (
  startNotificationDaemon
  ) where

import NotificationCenter.Notifications.Notification
  ( showNotificationWindow
  , Notification(..)
  , DisplayingNotificaton(..)
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

import GI.Gdk (threadsEnter, threadsLeave)

data NotifyState = NotifyState
  { notiStList :: [Notification]
  , notiDisplayingList :: [DisplayingNotificaton]
  , notiStNextId :: Int
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
  istate' <- readTVarIO istate
  let newNoti = Notification
        { notiAppName = appName
        , notiRepId = replaceId
        , notiId = notiStNextId istate'
        , notiIcon = icon
        , notiSummary = summary
        , notiBody = body
        , notiActions = actions
        , notiHints = hints
        , notiTimeout = timeout
        }
  atomically $ modifyTVar istate $ \istate' ->
    istate' { notiStList = newNoti : notiStList istate'
            , notiStNextId = notiStNextId istate' + 1}
  -- new noti-window
  threadsEnter
  dnoti <- showNotificationWindow newNoti
    (notiDisplayingList istate')
    (removeNotiFromDistList istate $ notiId newNoti)
  threadsLeave
  atomically $ modifyTVar istate $ \istate' ->
    istate' { notiDisplayingList = dnoti : notiDisplayingList istate' }
  -- trigger update in noti-center
  return 0

removeNotiFromDistList istate id = do
  atomically $ modifyTVar istate $ \istate' ->
    istate' { notiDisplayingList =
              filter (\n -> (dNotiId n) /= id)
              (notiDisplayingList istate')}
  return ()

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
  istate <- newTVarIO $ NotifyState [] [] 0
  forkIO (notificationDaemon (notify istate))
  return ()

