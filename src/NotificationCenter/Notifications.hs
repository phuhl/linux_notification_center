{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications
  ( startNotificationDaemon
  , NotifyState(..)
  , Notification(..)
  ) where

import NotificationCenter.Notifications.Notification
  ( showNotificationWindow
  , Notification(..)
  , DisplayingNotificaton(..)
  , Urgency(..)
  )
import TransparentWindow

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  (readTVarIO, modifyTVar', TVar(..), atomically, newTVarIO)

import DBus ( Variant (..), fromVariant )
import DBus.Client
       ( connectSession, AutoMethod(..), autoMethod, requestName, export
       , nameAllowReplacement, nameReplaceExisting)
import Data.Text ( Text, pack )
import Data.Word ( Word8, Word32 )
import Data.Int ( Int32 )
import qualified Data.Map as Map ( Map, lookup )
import Data.Time
import Data.Time.LocalTime

import System.Locale.Read


data NotifyState = NotifyState
  { notiStList :: [ Notification ]
    -- ^ List of all notis
  , notiDisplayingList :: [ DisplayingNotificaton ]
    -- ^ List of all notis getting displayed as popup
  , notiStNextId :: Int
    -- ^ Id for the next noti
  , notiStOnUpdate :: IO ()
    -- ^ Update-function for the NotificationCenter
  }


getServerInformation :: IO (Text, Text, Text, Text)
getServerInformation =
  return ("haskell-notification-daemon",
          "abc",
          "0.0.1",
          "1.0")

getCapabilities :: IO [Text]
getCapabilities = return ["body", "body-markup", "hints"]

parseUrgency hints =
  let urgency = (do v <- Map.lookup "urgency" hints
                    fromVariant v) :: Maybe Word8
  in
    case urgency of
      (Just 0) -> Low
      Nothing  -> Normal
      (Just 1) -> Normal
      (Just 2) -> High


getTime = do
  now <- zonedTimeToLocalTime <$> getZonedTime
  zone <- System.Locale.Read.getCurrentLocale
  let format = pack . flip (formatTime zone) now
  return $ format "%H:%M"


notify :: TVar NotifyState
          -> Text -- ^ Application name
          -> Word32 -- ^ Replaces id
          -> Text -- ^ App icon
          -> Text -- ^ Summary
          -> Text -- ^ Body
          -> [Text] -- ^ Actions
          -> Map.Map Text Variant -- ^ Hints
          -> Int32 -- ^ Expires timeout (milliseconds)
          -> IO Word32
notify istate appName replaceId icon summary body
  actions hints timeout = do
  addSource $ do
    istate' <- readTVarIO istate
    time <- getTime
    let newNoti = Notification
          { notiAppName = appName
          , notiRepId = replaceId
          , notiId = notiStNextId istate'
          , notiIcon = icon
          , notiSummary = summary
          , notiBody = body
          , notiActions = actions
          , notiHints = hints
          , notiUrgency = parseUrgency hints
          , notiTimeout = timeout
          , notiTime = time
          }
    atomically $ modifyTVar' istate $ \istate' ->
      istate' { notiStList = newNoti : notiStList istate'
              , notiStNextId = notiStNextId istate' + 1}
    -- new noti-window
    dnoti <- showNotificationWindow newNoti
      (notiDisplayingList istate')
      (removeNotiFromDistList istate $ notiId newNoti)
    atomically $ modifyTVar' istate $ \istate' ->
      istate' { notiDisplayingList = dnoti : notiDisplayingList istate' }
    -- trigger update in noti-center
    notiStOnUpdate istate'
    return False
  return 0

removeNotiFromDistList istate id = do
  atomically $ modifyTVar' istate $ \istate' ->
    istate' { notiDisplayingList =
              filter (\n -> (dNotiId n) /= id)
              (notiDisplayingList istate')}
  return ()

notificationDaemon :: (AutoMethod f) => f -> IO ()
notificationDaemon onNote = do
  putStrLn "notificationDaemon started"
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

startNotificationDaemon :: IO () ->  IO (TVar NotifyState)
startNotificationDaemon onUpdate = do
  istate <- newTVarIO $ NotifyState [] [] 0 onUpdate
  forkIO (notificationDaemon (notify istate))
  return istate

