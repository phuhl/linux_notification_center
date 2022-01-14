{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter where

import Config
  (getConfig,Config(..))
import NotificationCenter.NotificationInCenter
  (DisplayingNotificationInCenter(..), showNotification, updateNoti)
import NotificationCenter.Notifications
  (NotifyState(..), startNotificationDaemon, hideAllNotis)
import NotificationCenter.Notifications.Data (Notification(..))
import NotificationCenter.Glade (glade)
import NotificationCenter.Button
  (Button(..), createButton, setButtonState)
import TransparentWindow
import Helpers

import Prelude

import System.Locale.SetLocale
import System.IO.Unsafe
import System.IO (readFile)

import Data.Int (Int32(..))
import Data.Tuple.Sequence (sequenceT)
import Data.Maybe
import Data.IORef
import Data.Gettext
import Data.List
import Data.Time
import Data.Time.LocalTime
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Complex
import Data.Monoid ((<>))

import Control.Monad
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay, ThreadId(..))
import Control.Concurrent.STM
  ( readTVarIO, modifyTVar', TVar(..), atomically, newTVarIO )

import System.Process (runCommand)
import System.Locale.Current
import System.Posix.Signals (sigUSR1)
import System.Posix.Daemonize (serviced, daemonize)
import System.Directory (getXdgDirectory, XdgDirectory(..))

import DBus ( fromVariant )

import GI.Gtk
       (buttonSetLabel, widgetSetHalign, widgetSetHexpand, buttonNew, setWidgetMargin, buttonSetRelief, widgetSetSizeRequest, widgetShowAll, widgetShow, widgetHide, onWidgetDestroy
       , windowSetDefaultSize, setWindowTitle, boxPackStart, boxNew
       , setWindowWindowPosition, WindowPosition(..), windowMove
       , frameSetShadowType, aspectFrameNew
       , widgetGetAllocatedHeight, widgetGetAllocatedWidth, onWidgetDraw
       , adjustmentSetValue, adjustmentGetLower, adjustmentGetUpper, adjustmentGetPageSize
       , onWidgetLeaveNotifyEvent, onWidgetMotionNotifyEvent
       , widgetAddEvents, alignmentSetPadding, alignmentNew, rangeSetValue
       , scaleSetDigits, scaleSetValuePos, rangeGetValue
       , afterScaleButtonValueChanged, scaleNewWithRange, containerAdd
       , buttonBoxNew, mainQuit, onButtonActivate
       , toggleButtonGetActive, onToggleButtonToggled, buttonSetUseStock
       , toggleButtonNewWithLabel, onButtonClicked
       , buttonNewWithLabel, widgetQueueDraw, drawingAreaNew
       , windowNew, widgetDestroy, dialogRun, setAboutDialogComments
       , setAboutDialogAuthors, setAboutDialogVersion
       , setAboutDialogProgramName, aboutDialogNew, labelNew, get
       , afterWindowSetFocus, labelSetText
       , onWidgetFocusOutEvent, onWidgetKeyReleaseEvent, widgetGetParentWindow
       , onButtonClicked, windowGetScreen, boxNew, widgetSetValign)
import qualified GI.Gtk as Gtk (containerAdd, Window(..), Box(..), Label(..), Button(..), Adjustment(..))

import qualified GI.Gtk as GI (init, main)
import GI.GLib (sourceRemove, timeoutAdd, unixSignalAdd)
import GI.GLib.Constants
import GI.Gdk.Constants
import GI.Gdk.Flags (EventMask(..))
import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ShadowType(..)
       , PositionType(..), ReliefStyle(..), Align(..))
import Data.GI.Base.BasicConversions (gflagsToWord)
import qualified GI.Gdk.Objects.Window


data State = State
  { stMainWindow :: Gtk.Window
  , stNotiBox :: Gtk.Box
  , stNotiBoxAdj :: Gtk.Adjustment
  , stTimeLabel :: Gtk.Label
  , stDateLabel :: Gtk.Label
  , stDeleteAll :: Gtk.Button
  , stUserButtons :: [ Button ]
  , stNotiState :: TVar NotifyState
  , stDisplayingNotiList :: [ DisplayingNotificationInCenter ]
  , stNotisForMe :: [ Notification ]
  , stCenterShown :: Bool
  , stPopupsPaused :: Bool
  }


setTime :: TVar State -> IO Bool
setTime tState = do
  state <- readTVarIO tState
  now <- zonedTimeToLocalTime <$> getZonedTime
  zone <- System.Locale.Current.currentLocale
  let format = Text.pack . flip (formatTime zone) now
  labelSetText (stTimeLabel state) $ format "%H:%M"
  labelSetText (stDateLabel state) $ format "%A, %x"
  return False

startSetTimeThread :: TVar State -> IO ()
startSetTimeThread tState = do
  runAfterDelay 1000 (startSetTimeThread' tState)
  return ()

startSetTimeThread' :: TVar State -> IO ()
startSetTimeThread' tState = do
  addSource (setTime tState)
  time <- fromIntegral <$> diffTimeToPicoseconds <$> utctDayTime <$> getCurrentTime
  let delay = (60 * 1000000) - ((ceiling (time / 1000000)) `mod` (1000000 * 60))
  runAfterDelay delay (startSetTimeThread' tState)
  return ()

deleteInCenter tState = do
  displayList <- stDisplayingNotiList <$> readTVarIO tState
  mapM (removeNoti tState) displayList
  return ()

setWindowStyle tState = do
  state <- readTVarIO tState
  homeDir <- getXdgDirectory XdgConfig ""
  style <- readFile (homeDir ++ "/deadd/deadd.css")
  screen <- windowGetScreen $ stMainWindow state
  setStyle screen $ BS.pack $ style
  return False

createNotiCenter :: TVar State -> Config -> Catalog -> IO ()
createNotiCenter tState config catalog = do
  (objs, _) <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "label_time"
    , "label_date"
    , "box_notis"
    , "box_notis_adj"
    , "box_buttons"
    , "button_deleteAll" ]
    (Just "Notification area")

  mainWindow <- window objs "main_window"
  notiBox <- box objs "box_notis"
  buttonBox <- box objs "box_buttons"
  timeL <- label objs "label_time"
  timeD <- label objs "label_date"
  notiBoxAdj <- adjustment objs "box_notis_adj"
  deleteButton <- button objs "button_deleteAll"

  onButtonClicked deleteButton $ deleteInCenter tState
  buttonSetLabel deleteButton $ LT.toStrict $ gettext catalog "Delete all"

  let buttons = zip
        (split $ removeOuterLetters $ configLabels config)
        (split $ removeOuterLetters $ configCommands config)
      (buttonLabels, buttonCommands) = unzip buttons
      margin = fromIntegral $ configButtonMargin config
      width = fromIntegral (((configWidth config) - 20)
                            `div` (configButtonsPerRow config))
              - margin * 2
      height = fromIntegral $ configButtonHeight config
      linesNeeded = fromIntegral $ ceiling
        $ ((fromIntegral $ length buttons) / (fromIntegral $ configButtonsPerRow config))
  lines' <- sequence $ take linesNeeded $ repeat $ boxNew OrientationHorizontal 0
  buttons' <- sequence $ map
    (\(label, command) -> createButton config width height command label)
    buttons


  sequence $ map (\(box, buttons'') ->
                    sequence $ Gtk.containerAdd box <$> buttons'')
    $ zip (reverse lines') (splitEvery (configButtonsPerRow config)
                            $ (buttonButton <$> buttons'))
  sequence $ Gtk.containerAdd buttonBox <$> lines'
  sequence $ widgetShowAll <$> lines'

  atomically $ modifyTVar' tState $
    \state -> state { stMainWindow = mainWindow
                      , stNotiBox = notiBox
                      , stNotiBoxAdj = notiBoxAdj
                      , stTimeLabel = timeL
                      , stDateLabel = timeD
                      , stDeleteAll = deleteButton
                      , stNotisForMe = []
                      , stUserButtons = buttons' }

  setWindowStyle tState

  startSetTimeThread tState


  when (configNotiCenterHideOnMouseLeave config) $ do
    onWidgetLeaveNotifyEvent mainWindow $ \(_) -> do
      hideNotiCenter tState
      return True
    return ()

  setNotificationCenterPosition mainWindow config

  onWidgetDestroy mainWindow mainQuit

  return ()

setNotificationCenterPosition mainWindow config = do

  (screenW, screenY, screenH) <- if configNotiCenterFollowMouse config then
    getMouseActiveScreenPos mainWindow (fromIntegral $ configNotiMonitor config)
    else
    getScreenPos mainWindow (fromIntegral $ configNotiCenterMonitor config)

  windowSetDefaultSize mainWindow
    width -- w
    (screenH - barHeightTop - barHeightBottom) -- h
  windowMove mainWindow
    (screenW - width - marginRight) -- x
    (screenY + barHeightTop)  -- y
  return ()
    where
      barHeightTop = fromIntegral $ configBarHeight config
      barHeightBottom = fromIntegral $ configBottomBarHeight config
      marginRight = fromIntegral $ configRightMargin config
      width = fromIntegral $ configWidth config

parseButtons noti tState = do
  state <- readTVarIO tState
  let buttons = stUserButtons state
      maybeId = (Map.lookup "id" noti >>= fromVariant) :: Maybe Int32
      id = checkId maybeId
        where
          checkId (Just id)
            | id < (fromIntegral $ length buttons) = Just id
            | otherwise = Nothing
          checkId Nothing = Nothing
      maybeState = Map.lookup "state" noti >>= fromVariant :: Maybe Bool
      maybeButton = (!!) buttons <$> (fromIntegral <$> id)
  fromMaybe (return ()) $ setButtonState <$> maybeButton <*> maybeState
  return ()


parseNotisForMe tState = do
  state <- readTVarIO tState
  -- do stuff
  let notisForMe = stNotisForMe state
      myNotiHints = notiHints <$> notisForMe
  atomically $ modifyTVar' tState
    (\state -> state { stNotisForMe = [] })
  sequence $ map (\noti -> do
          let maybeType = Map.lookup "type" noti
            in case (maybeType >>= fromVariant) :: Maybe (String) of
                 (Just "clearInCenter") -> do
                   putStrLn "clearing in center"
                   deleteInCenter tState
                 (Just "clearPopups") -> do
                   putStrLn "clearing popups"
                   hideAllNotis $ stNotiState state
                 (Just "buttons") -> parseButtons noti tState
                 Nothing -> parseButtons noti tState
                 (Just "pausePopups") -> do
                   putStrLn "pausing popups"
                   atomically $ modifyTVar' tState
                     (\state -> state { stPopupsPaused = True })
                 (Just "unpausePopups") -> do
                   putStrLn "unpausing popups"
                   atomically $ modifyTVar' tState
                     (\state -> state { stPopupsPaused = False })
                 (Just "reloadStyle") -> do
                   putStrLn "Reloading Style"
                   addSource $ setWindowStyle tState
                   return ()
      ) myNotiHints
  return False

hideNotiCenter tState = do
  state <- readTVarIO tState
  mainWindow <- stMainWindow <$> readTVarIO tState
  widgetHide mainWindow
  atomically $ modifyTVar' tState
    (\state -> state { stCenterShown = False })


showNotiCenter tState notiState config = do
  state <- readTVarIO tState
  mainWindow <- stMainWindow <$> readTVarIO tState
  setNotificationCenterPosition mainWindow config
  newShown <- if stCenterShown state then
    do
      widgetHide mainWindow
      return False
    else
    do
      hideAllNotis $ stNotiState state
      widgetShow mainWindow
      return True
  atomically $ modifyTVar' tState
    (\state -> state {stCenterShown = newShown })

  notiBoxAdj <- stNotiBoxAdj <$> readTVarIO tState
  notiBoxAdjVal <- if configNotiCenterNewFirst config then
    do
      adj <- adjustmentGetLower notiBoxAdj
      return adj
    else
    do
      adj <- adjustmentGetUpper notiBoxAdj
      page <- adjustmentGetPageSize notiBoxAdj
      return (adj - page)
  adjustmentSetValue notiBoxAdj notiBoxAdjVal

  return True

updateNotisForMe :: TVar State -> IO()
updateNotisForMe tState = do
  state <- readTVarIO tState
  notiState <- readTVarIO $ stNotiState state
  atomically $ modifyTVar' (stNotiState state) (
    \state -> state { notiForMeList = [] })
  atomically $ modifyTVar' tState (
    \state -> state { stNotisForMe = notiForMeList notiState
                                     ++ stNotisForMe state})

  addSource (parseNotisForMe tState)
  return ()

updateNotis :: Config -> TVar State -> IO()
updateNotis config tState = do
  state <- readTVarIO tState
  notiState <- readTVarIO $ stNotiState state

  let newNotis = filter (
        \n -> (find (\nd -> _dNotiId nd == notiId n )
               (stDisplayingNotiList state))
              == Nothing
              && ((configIgnoreTransient config) || (not $ notiTransient n)))
                 $ notiStList notiState
  newNotis' <- mapM (
    \n -> do
      let newNoti = DisplayingNotificationInCenter
                    { _dNotiId = notiId n }
      showNotification config
        (stNotiBox state) newNoti
        (stNotiState state) $ removeNoti tState
    ) newNotis

  let delNotis = filter (\nd -> (find (\n -> _dNotiId nd == notiId n)
                                $ notiStList notiState) == Nothing)
                 $ stDisplayingNotiList state
  atomically $ modifyTVar' tState (
    \state -> state { stDisplayingNotiList =
                      newNotis' ++ stDisplayingNotiList state
                    })
  setDeleteAllState tState
  mapM (removeNoti tState) $ delNotis
  when (stCenterShown state || stPopupsPaused state) $
    do
      hideAllNotis $ stNotiState state
  return ()


removeNoti :: TVar State -> DisplayingNotificationInCenter -> IO ()
removeNoti tState dNoti = do
  state <- readTVarIO tState
  atomically $ modifyTVar' tState $ \state ->
    state { stDisplayingNotiList =
            filter ((/=) dNoti) $ stDisplayingNotiList state}
  setDeleteAllState tState
  atomically $ modifyTVar' (stNotiState state) $ \state ->
    state { notiStList = filter
                         (\n -> notiId n /= _dNotiId dNoti) $
                         notiStList state }
  addSource $ do
    _dNotiDestroy dNoti
    return False
  return ()

setDeleteAllState tState = do
  addSource $ do
    state <- readTVarIO tState
    if (length $ stDisplayingNotiList state) > 1 then
      widgetShow $ stDeleteAll state
      else
      widgetHide $ stDeleteAll state
    return False
  return ()

getInitialState = do
  newTVarIO $ State
    { stDisplayingNotiList = []
    , stCenterShown = False
    , stPopupsPaused = False}

main' :: IO ()
main' = do
  GI.init Nothing

  homeDir <- getXdgDirectory XdgConfig ""
  config <- getConfig <$> (readConfigFile
                            (homeDir ++ "/deadd/deadd.conf"))

  catalog <- getCatalog

  istate <- getInitialState
  notiState <- startNotificationDaemon config
    (updateNotis config istate) (updateNotisForMe istate)

  atomically $ modifyTVar' istate $
    \istate' -> istate' { stNotiState = notiState }
  createNotiCenter istate config catalog

  unixSignalAdd PRIORITY_HIGH (fromIntegral sigUSR1)
    (showNotiCenter istate notiState config)

  runCommand $ configStartupCommand config

  GI.main

main :: IO ()
main = do
--  daemonize main'
  main'
