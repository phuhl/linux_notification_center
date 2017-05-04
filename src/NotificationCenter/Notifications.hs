{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications
  ( startNotificationDaemon
  , NotifyState(..)
  , Notification(..)
  , hideAllNotis
  ) where

import NotificationCenter.Notifications.Notification
  ( showNotificationWindow
  , updateNoti
  , Notification(..)
  , DisplayingNotificaton(..)
  )
import NotificationCenter.Notifications.Data
  (Urgency(..))
import TransparentWindow
import NotificationCenter.Notifications.Data

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  (readTVarIO, modifyTVar, modifyTVar', TVar(..), atomically, newTVarIO)

import DBus ( Variant (..), fromVariant )
import DBus.Client
       ( connectSession, AutoMethod(..), autoMethod, requestName, export
       , nameAllowReplacement, nameReplaceExisting)
import Data.Text ( Text, pack )
import Data.Word ( Word8, Word32 )
import Data.Int ( Int32 )
import Data.List
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
  , notiConfig :: Config
  }


getServerInformation :: IO (Text, Text, Text, Text)
getServerInformation =
  return ("haskell-notification-daemon",
          "abc",
          "0.0.1",
          "1.0")

getCapabilities :: IO [Text]
getCapabilities = return ["body", "body-markup", "hints", "persistence"]

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
notify tState appName replaceId icon summary body
  actions hints timeout = do
  state <- readTVarIO tState
  time <- getTime
  let newNoti = Notification
        { notiAppName = appName
        , notiRepId = replaceId
        , notiId = notiStNextId state
        , notiIcon = icon
        , notiSummary = summary
        , notiBody = body
        , notiActions = actions
        , notiHints = hints
        , notiUrgency = parseUrgency hints
        , notiTimeout = timeout
        , notiTime = time
        }
  let notis = filter (\n -> dNotiId n ==
                       fromIntegral (notiRepId newNoti))
                $ notiDisplayingList state
  atomically $ modifyTVar' tState $ \state ->
    state { notiStList = updatedNotiList (notiStList state)
                         newNoti (fromIntegral (notiRepId newNoti))
          , notiStNextId = notiStNextId state + 1}
  if length notis == 0 then
    insertNewNoti newNoti tState
    else
    replaceNoti newNoti tState
  return $ fromIntegral $ notiId newNoti
    where
      updatedNotiList :: [Notification] -> Notification
                      -> Int -> [Notification]
      updatedNotiList notis newNoti repId =
        let notis' = map (\n -> if notiId n == repId then newNoti
                                else n) notis
        in if (find ((==) newNoti) notis') /= Nothing then notis'
           else (newNoti:notis')


replaceNoti newNoti tState = do
  addSource $ do
    atomically $ modifyTVar tState $ \state ->
      state { notiDisplayingList = map
              (\n -> if dNotiId n /= repId then n
                     else n { dNotiId = notiId newNoti } )
              $ notiDisplayingList state}
    state <- readTVarIO tState
    let notis = filter (\n -> dNotiId n == notiId newNoti)
                $ notiDisplayingList state
    mapM (updateNoti (notiConfig state)
           (removeNotiFromDistList tState $ notiId newNoti)
           newNoti) notis
    notiStOnUpdate state
    return False
      where repId = fromIntegral (notiRepId newNoti)

insertNewNoti newNoti tState = do
  addSource $ do
    state <- readTVarIO tState
    -- new noti-window
    dnoti <- showNotificationWindow
      (notiConfig state)
      newNoti
      (notiDisplayingList state)
      (removeNotiFromDistList tState $ notiId newNoti)
    atomically $ modifyTVar' tState $ \state ->
      state { notiDisplayingList = dnoti : notiDisplayingList state }
    -- trigger update in noti-center
    notiStOnUpdate state
    return False


removeNotiFromDistList tState id = do
  state <- readTVarIO tState
  atomically $ modifyTVar' tState $ \state ->
    state { notiDisplayingList =
              filter (\n -> (dNotiId n) /= id)
              (notiDisplayingList state)}
  addSource $ do
    let mDn = find (\n -> dNotiId n == id) $ notiDisplayingList state
    maybe (return ()) dNotiDestroy mDn
    return False
  return ()

hideAllNotis tState = do
  state <- readTVarIO tState
  mapM (removeNotiFromDistList tState . dNotiId)
    $ notiDisplayingList state
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

startNotificationDaemon :: Config -> IO () ->  IO (TVar NotifyState)
startNotificationDaemon config onUpdate = do
  istate <- newTVarIO $ NotifyState [] [] 1 onUpdate config
  forkIO (notificationDaemon (notify istate))
  return istate

