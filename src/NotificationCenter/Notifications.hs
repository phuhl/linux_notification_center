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
  (readTVarIO, stateTVar, modifyTVar, modifyTVar', TVar(..),
   atomically, newTVarIO)

import DBus ( Variant (..), fromVariant )
import DBus.Client
       ( connectSession, AutoMethod(..), autoMethod, requestName, export
       , nameAllowReplacement, nameReplaceExisting)
import Data.Text ( Text, pack )
import Data.Word ( Word, Word8, Word32 )
import Data.Int ( Int32 )
import Data.List
import qualified Data.Map as Map
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
  , notiStOnUpdateForMe :: IO ()
    -- ^ Update-function for the NotificationCenter for notifications
    -- for internal use
  , notiConfig :: Config
    -- ^ Configuration
  , notiForMeList :: [ Notification ]
    -- ^ Notifications that should not be displayed but are used as
    --   as hints for this notification daemon
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


parseTransient :: Map.Map Text Variant -> Bool
parseTransient hints =
  let transient = Map.lookup "transient" hints
  in case transient of
    Nothing -> False
    (Just b) -> True

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
  let newNotiWithoutId = Notification
        { notiAppName = appName
        , notiRepId = replaceId
        -- in order to not create a race condition this can not be
        -- done, instead it is handled lower done
        , notiId = 0
        , notiIcon = icon
        , notiSummary = summary
        , notiBody = body
        , notiActions = actions
        , notiHints = hints
        , notiUrgency = parseUrgency hints
        , notiTimeout = timeout
        , notiTime = time
        , notiTransient = parseTransient hints
        }

  if Map.member (pack "deadd-notification-center")
    $ notiHints newNotiWithoutId
    then
    -- Noti is for messaging the noti-center
    do
      atomically $ modifyTVar' tState $ \state ->
        state { notiForMeList = newNotiWithoutId:(notiForMeList state) }
      notiStOnUpdateForMe state
      return $ fromIntegral 0
    else
    -- Noti has to be displayed
    do
      let notisToBeReplaced = filter (\n -> dNotiId n ==
                                       fromIntegral (notiRepId newNotiWithoutId))
                              $ notiDisplayingList state
      newId <- atomically $ stateTVar tState
               $ \state -> (notiStNextId state + 1,
                            state { notiStList =
                                    updatedNotiList (notiStList state)
                                    (newNotiWithoutId { notiId = notiStNextId state })
                                    (fromIntegral (notiRepId newNotiWithoutId))
                                  , notiStNextId = notiStNextId state + 1})
      let newNotiWithId = newNotiWithoutId { notiId = newId }
      if length notisToBeReplaced == 0 then
        insertNewNoti newNotiWithId tState
        else
        replaceNoti newNotiWithId tState
      return $ fromIntegral $ notiId newNotiWithId
        where
          updatedNotiList :: [Notification] -> Notification
                          -> Int -> [Notification]
          updatedNotiList oldNotis newNoti repId =
            let notis' = map (\n -> if notiId n == repId then newNoti
                                    else n) oldNotis
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


removeNotiFromDistList' :: TVar NotifyState -> Word32 -> IO ()
removeNotiFromDistList' tState id =
  removeNotiFromDistList tState $ fromIntegral id

removeNotiFromDistList :: TVar NotifyState -> Int -> IO ()
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

notificationDaemon :: (AutoMethod f1, AutoMethod f2) =>
                      f1 -> f2 -> IO ()
notificationDaemon onNote onCloseNote = do
  putStrLn "notificationDaemon started"
  client <- connectSession
  _ <- requestName client "org.freedesktop.Notifications"
       [nameAllowReplacement, nameReplaceExisting]
  export client "/org/freedesktop/Notifications"
    [ autoMethod "org.freedesktop.Notifications"
      "GetServerInformation" getServerInformation
    , autoMethod "org.freedesktop.Notifications"
      "GetCapabilities" getCapabilities
    , autoMethod "org.freedesktop.Notifications"
      "CloseNotification" onCloseNote
    , autoMethod "org.freedesktop.Notifications"
      "Notify" onNote
    ]

startNotificationDaemon :: Config -> IO () ->  IO () ->  IO (TVar NotifyState)
startNotificationDaemon config onUpdate onUpdateForMe = do
  istate <- newTVarIO $ NotifyState [] [] 1 onUpdate onUpdateForMe config []
  forkIO (notificationDaemon (notify istate)
           (removeNotiFromDistList' istate))
  return istate

