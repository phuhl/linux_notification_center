{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications
  ( startNotificationDaemon
  , NotifyState(..)
  , hideAllNotis
  ) where

import Helpers (trim, isPrefix, splitOn, atMay, eitherToMaybe
               , removeImgTag, removeAllTags, parseHtmlEntities )

import NotificationCenter.Notifications.NotificationPopup
  ( showNotificationWindow
  , updateNoti
  , DisplayingNotificationPopup(..)
  )
import NotificationCenter.Notifications.Data
  (Urgency(..), Notification(..), Image(..), parseImageString)
import TransparentWindow
import Config (Config(..))
import NotificationCenter.Notifications.Data

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  (readTVarIO, stateTVar, modifyTVar, modifyTVar', TVar(..),
   atomically, newTVarIO)

import DBus (Variant(..), Structure(..), fromVariant, signal, toVariant, variantType)
import DBus.Internal.Message (Signal(..))
import DBus.Client
       ( connectSession, AutoMethod(..), autoMethod, requestName, export
       , nameAllowReplacement, nameReplaceExisting, emit)
import Data.Char (toLower)
import Data.Text (unpack, Text, pack )
import qualified Data.Text as Text
import Data.Word ( Word, Word8, Word32 )
import Data.Int ( Int32 )
import Data.List
import qualified Data.Map as Map
import Data.Time
import Data.Time.LocalTime
import Data.Maybe (fromMaybe)

import System.Locale.Read
import System.IO (readFile)
import System.IO.Error (tryIOError)
import Data.GI.Base.GError (catchGErrorJust)

import GI.Gio.Interfaces.AppInfo (appInfoGetIcon, appInfoGetAll, appInfoGetName)
import GI.Gio.Interfaces.Icon (iconToString, Icon(..))

data NotifyState = NotifyState
  { notiStList :: [ Notification ]
    -- ^ List of all notis
  , notiDisplayingList :: [ DisplayingNotificationPopup ]
    -- ^ List of all notis getting displayed as popup
  , notiStNextId :: Int
    -- ^ Id for the next noti
  , notiStOnUpdate :: IO ()
    -- ^ Update-function for the NotificationCenter
  , notiStOnUpdateForMe :: IO ()
    -- ^ Update-function for the NotificationCenter for notifications
    --   for internal use
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

getCapabilities :: Config -> IO [Text]
getCapabilities config = return ( [ "body"
                                  , "hints"
                                  , "actions"
                                  , "persistence"
                                  , "icon-static"
                                  , "action-icons" ]
                                  ++ if (configNotiMarkup config) then
                                    [ "body-markup"
                                    , "body-hyperlinks"] else [])

emitNotificationClosed :: Bool -> (Signal -> IO ()) -> Int -> CloseType -> IO ()
emitNotificationClosed doSend onClose id ctype =
  if doSend then
    onClose $ (signal "/org/freedesktop/Notifications"
               "org.freedesktop.Notifications"
               "NotificationClosed")
    { signalBody = [ toVariant (fromIntegral id :: Word32)
                   , toVariant (case ctype of
                                   Timeout -> 1
                                   User -> 2
                                   CloseByCall -> 3
                                   _ -> 4 :: Word32)] }
  else return ()

emitAction :: (Signal -> IO ()) -> Int -> String -> IO ()
emitAction onAction id key = do
  onAction $ (signal "/org/freedesktop/Notifications"
               "org.freedesktop.Notifications"
               "ActionInvoked")
    { signalBody = [ toVariant (fromIntegral id :: Word32)
                   , toVariant key] }


parseActionIcons hints =
  fromMaybe False $ (fromVariant =<< Map.lookup "action-icons" hints :: Maybe Bool)

parseUrgency hints =
  let urgency = fromVariant =<< Map.lookup "urgency" hints :: Maybe Word8
  in case urgency of
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

getDesktopFile :: String -> IO (Maybe String)
getDesktopFile name = do
  [try1, try2, try3] <- sequence [ getIt "~/.local/share/applications/"
                                 , getIt "/usr/local/share/applications/"
                                 , getIt "/usr/share/applications/"]
  return $ try1 <|> try2 <|> try3
  where getIt path = eitherToMaybe <$>
          (tryIOError $ readFile $ path ++ name ++ ".desktop")

getAppIcon :: String -> IO Image
getAppIcon name = do
  apps <- appInfoGetAll
  names <- sequence $ appInfoGetName <$> apps
  let appInfos = filter (\(_, name') -> name' == (pack name))
              $ zip apps names
  if length appInfos > 0 then do
    mIcon <- appInfoGetIcon (fst $ appInfos !! 0)
    case mIcon of
      (Just icon) -> imgFromMaybe <$> ((<$>) unpack) <$> iconToString icon
      Nothing -> return NoImage
    else
    return NoImage
  where imgFromMaybe mI= case mI  of
                           Nothing -> NoImage
                           (Just w) -> if isPrefix "/" w then
                             ImagePath w else NamedIcon w

parseIcon :: Config -> Map.Map Text Variant -> Text -> Text -> IO Image
parseIcon config hints icon appName =
  if (Text.length icon) > 0 then do
    return $ parseImageString icon
    else do
      let mFileName = fromVariant =<< Map.lookup "desktop-entry" hints
        in case mFileName of
         (Just fileName) -> do
           getAppIcon fileName

         Nothing -> if configGuessIconFromAppname config then
                      getAppIcon $ unpack appName
                    else
                      return NoImage

parseImg :: Map.Map Text Variant -> Text -> Image
parseImg hints text =
  fromMaybe NoImage
  $ fromBody <|> fromImageData <|> fromImagePath <|> fromIcon
  where
    fromBody = ImagePath <$> lookup "src" (snd $ removeImgTag (unpack text))
    fromIcon = RawImg <$> (fromVariant =<< Map.lookup "icon_data" hints)
    fromImageData = RawImg <$> (fromVariant =<< Map.lookup "image-data" hints)
    fromImagePath = parseImageString <$> (fromVariant =<< Map.lookup "image-path" hints)

getTime = do
  now <- zonedTimeToLocalTime <$> getZonedTime
  zone <- System.Locale.Read.getCurrentLocale
  let format = pack . flip (formatTime zone) now
  return $ format "%H:%M"

htmlEntitiesStrip :: Config -> Text -> Text
htmlEntitiesStrip config text = 
  if configNotiParseHtmlEntities config
  then Text.pack $ parseHtmlEntities $ unpack text
  else text

xmlStrip :: Config -> Text -> Text
xmlStrip config text = do
  if configNotiMarkup config then
    Text.pack $ fst $ removeImgTag $ unpack text
    else Text.pack $ removeAllTags $ unpack text


notify :: Config
       -> TVar NotifyState
       -> (Signal -> IO ())
       -> Text -- ^ Application name
       -> Word32 -- ^ Replaces id
       -> Text -- ^ App icon
       -> Text -- ^ Summary
       -> Text -- ^ Body
       -> [Text] -- ^ Actions
       -> Map.Map Text Variant -- ^ Hints
       -> Int32 -- ^ Expires timeout (milliseconds)
       -> IO Word32
notify config tState emit
  appName replaceId icon summary body actions hints timeout = do
  state <- readTVarIO tState
  time <- getTime
  icon <- parseIcon config hints icon appName
  let newNotiWithoutId = Notification
        { notiAppName = appName
        , notiRepId = replaceId

        -- in order to not create a race condition this can not be
        -- done, instead it is handled lower done
        , notiId = 0
        , notiIcon = icon
        , notiImg = parseImg hints body
        , notiSummary = htmlEntitiesStrip config summary
        , notiBody = htmlEntitiesStrip config $ xmlStrip config body
        , notiActions = actions
        , notiActionIcons = parseActionIcons hints
        , notiHints = hints
        , notiUrgency = parseUrgency hints
        , notiTimeout = timeout
        , notiTime = time
        , notiTransient = parseTransient hints
        , notiSendClosedMsg = (configSendNotiClosedDbusMessage config)
        , notiTop = Nothing
        , notiRight = Nothing
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
      -- Apply modifications and run scripts for noti
      let matchingRules = filter (\(match, rep, com) -> match newNotiWithoutId)
            (configMatchingRules config)
      let newNotiWoIdModified = foldl (\noti (_, mod, _) -> mod noti)
            newNotiWithoutId matchingRules

      let newNoti = newNotiWoIdModified

      let notisToBeReplaced = filter (\n -> _dNotiId n ==
                                       fromIntegral (notiRepId newNoti))
                              $ notiDisplayingList state
      newId <- atomically $ stateTVar tState
               $ \state -> (
        notiStNextId state, state
          { notiStNextId = notiStNextId state + 1
          , notiStList =
              updatedNotiList (notiStList state)
              (newNoti
               { notiId = notiStNextId state
               , notiOnClosed = emitNotificationClosed (notiSendClosedMsg newNoti)
                 emit (notiStNextId state)
               , notiOnAction = emitAction
                                emit (notiStNextId state) })
              (fromIntegral (notiRepId newNoti))
          })
      let newNotiWithId = newNoti
            { notiId = newId
            , notiOnClosed = emitNotificationClosed (notiSendClosedMsg newNoti)
                             emit newId
            , notiOnAction = emitAction emit newId }
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
              (\n -> if _dNotiId n /= repId then n
                     else n { _dNotiId = notiId newNoti } )
              $ notiDisplayingList state}
    state <- readTVarIO tState
    let notis = filter (\n -> _dNotiId n == notiId newNoti)
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
              filter (\n -> (_dNotiId n) /= id)
              (notiDisplayingList state)}
  addSource $ do
    let mDn = find (\n -> _dNotiId n == id) $ notiDisplayingList state
    maybe (return ()) _dNotiDestroy mDn
    return False
  return ()

hideAllNotis tState = do
  state <- readTVarIO tState
  mapM (removeNotiFromDistList tState . _dNotiId)
    $ notiDisplayingList state
  return ()

closeNotification tState id = do
  state <- readTVarIO tState
  let notis = filter (\n -> (notiId n) /= fromIntegral id) (notiStList state)
  sequence $ (\noti -> addSource $ do notiOnClosed noti $ CloseByCall
                                      return False) <$> notis
  removeNotiFromDistList' tState id


notificationDaemon :: (AutoMethod f1, AutoMethod f2) =>
                      Config -> ((Signal -> IO ()) -> f1) -> f2 -> IO ()
notificationDaemon config onNote onCloseNote = do
  putStrLn "notificationDaemon started"
  client <- connectSession
  _ <- requestName client "org.freedesktop.Notifications"
       [nameAllowReplacement, nameReplaceExisting]
  export client "/org/freedesktop/Notifications"
    [ autoMethod "org.freedesktop.Notifications"
      "GetServerInformation" getServerInformation
    , autoMethod "org.freedesktop.Notifications"
      "GetCapabilities" (getCapabilities config)
    , autoMethod "org.freedesktop.Notifications"
      "CloseNotification" onCloseNote
    , autoMethod "org.freedesktop.Notifications"
      "Notify" (onNote (emit client))
    ]

startNotificationDaemon :: Config -> IO () ->  IO () ->  IO (TVar NotifyState)
startNotificationDaemon config onUpdate onUpdateForMe = do
  istate <- newTVarIO $ NotifyState [] [] 1 onUpdate onUpdateForMe config []
  forkIO (notificationDaemon config (notify config istate)
          (closeNotification istate))
  return istate

