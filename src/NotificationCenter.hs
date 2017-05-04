{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter where

import NotificationCenter.Notification
  (DisplayingNotificaton(..), showNotification, updateNoti)
import NotificationCenter.Notifications
  (NotifyState(..), startNotificationDaemon, Notification(..)
  , hideAllNotis)
import NotificationCenter.Glade (glade, style)
import TransparentWindow
import Helpers
import NotificationCenter.Notifications.Data

import Prelude

import Data.Maybe
import Data.IORef
import Data.List
import Data.Time
import Data.Time.LocalTime
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import Data.Complex
import Data.Monoid ((<>))

import Control.Monad
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay, ThreadId(..))
import Control.Concurrent.STM
  ( readTVarIO, modifyTVar', TVar(..), atomically, newTVarIO )

import System.Locale.Read
import System.Posix.Signals (sigUSR1)
import System.Posix.Daemonize (serviced, daemonize)
import System.Directory (getHomeDirectory)

import GI.Gtk
       (widgetShowAll, widgetShow, widgetHide, onWidgetDestroy
       , windowSetDefaultSize, setWindowTitle, boxPackStart, boxNew
       , setWindowWindowPosition, WindowPosition(..), windowMove
       , frameSetShadowType, aspectFrameNew
       , widgetGetAllocatedHeight, widgetGetAllocatedWidth, onWidgetDraw
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
       , onButtonClicked, windowGetScreen)
import qualified GI.Gtk as Gtk (Window(..), Box(..), Label(..), Button(..))

import qualified GI.Gtk as GI (init, main)
import GI.GLib (sourceRemove, timeoutAdd, unixSignalAdd)
import GI.GLib.Constants
import GI.Gdk.Constants
import GI.Gdk.Flags (EventMask(..))
import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ShadowType(..), PositionType(..))
import Data.GI.Base.BasicConversions (gflagsToWord)
import qualified GI.Gdk.Objects.Window


data State = State
  { stMainWindow :: Gtk.Window
  , stNotiBox :: Gtk.Box
  , stTimeLabel :: Gtk.Label
  , stDateLabel :: Gtk.Label
  , stDeleteAll :: Gtk.Button
  , stNotiState :: TVar NotifyState
  , stDisplayingNotiList :: [ DisplayingNotificaton ]
  , stCenterShown :: Bool
  }


setTime :: TVar State -> IO Bool
setTime tState = do
  state <- readTVarIO tState
  now <- zonedTimeToLocalTime <$> getZonedTime
  zone <- System.Locale.Read.getCurrentLocale
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


createNotiCenter :: TVar State -> Config -> IO ()
createNotiCenter tState config = do
  objs <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "label_time"
    , "label_date"
    , "box_notis"
    , "button_deleteAll" ]
    (Just "Notification area")

  mainWindow <- window objs "main_window"
  notiBox <- box objs "box_notis"
  timeL <- label objs "label_time"
  timeD <- label objs "label_date"
  deleteButton <- button objs "button_deleteAll"

  screen <- windowGetScreen mainWindow
  setStyle screen $ BS.pack style

--  (Just mainWindowGDK) <- widgetGetParentWindow mainWindow
  atomically $ modifyTVar' tState $
    \state -> state { stMainWindow = mainWindow
                      , stNotiBox = notiBox
                      , stTimeLabel = timeL
                      , stDateLabel = timeD
                      , stDeleteAll = deleteButton }

  startSetTimeThread tState


  onButtonClicked deleteButton $ do
    displayList <- stDisplayingNotiList <$> readTVarIO tState
    mapM (removeNoti tState) displayList
    return ()


  (screenH, screenW) <- getScreenProportions mainWindow

  onWidgetLeaveNotifyEvent mainWindow $ \(_) -> do
    hideNotiCenter tState
    return True

                                 -- w   h
  windowSetDefaultSize mainWindow 500 (screenH - barHeight)
  windowMove mainWindow (screenW - 500) barHeight
  onWidgetDestroy mainWindow mainQuit
  return ()
    where barHeight = fromIntegral $ configBarHeight config

hideNotiCenter tState = do
  state <- readTVarIO tState
  mainWindow <- stMainWindow <$> readTVarIO tState
  widgetHide mainWindow
  atomically $ modifyTVar' tState
    (\state -> state { stCenterShown = False })


showNotiCenter tState notiState = do
  state <- readTVarIO tState
  mainWindow <- stMainWindow <$> readTVarIO tState
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
  return True

updateNotis :: TVar State -> IO()
updateNotis tState = do
  state <- readTVarIO tState
  notiState <- readTVarIO $ stNotiState state

  let newNotis = filter (
        \n -> (find (\nd -> dNotiId nd == notiId n )
               (stDisplayingNotiList state))
              == Nothing) $ notiStList notiState
  newNotis' <- mapM (
    \n -> do
      let newNoti = DisplayingNotificaton
                    { dNotiId = notiId n }
      showNotification (stNotiBox state) newNoti
        (stNotiState state) $ removeNoti tState
    ) newNotis
  let delNotis = filter (\nd -> (find (\n -> dNotiId nd == notiId n)
                                $ notiStList notiState) == Nothing)
                 $ stDisplayingNotiList state
  atomically $ modifyTVar' tState (
    \state -> state { stDisplayingNotiList =
                      newNotis' ++ stDisplayingNotiList state})
  setDeleteAllState tState
  mapM (removeNoti tState) $ delNotis
  when (stCenterShown state) $
    do
      hideAllNotis $ stNotiState state
  return ()


removeNoti :: TVar State -> DisplayingNotificaton -> IO ()
removeNoti tState dNoti = do
  state <- readTVarIO tState
  atomically $ modifyTVar' tState $ \state ->
    state { stDisplayingNotiList =
            filter ((/=) dNoti) $ stDisplayingNotiList state}
  setDeleteAllState tState
  atomically $ modifyTVar' (stNotiState state) $ \state ->
    state { notiStList = filter
                         (\n -> notiId n /= dNotiId dNoti) $
                         notiStList state }
  addSource $ do
    dNotiDestroy dNoti
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

getConfig p =
  Config
    { configBarHeight = r 0 p nCenter "marginTop"
    , configNotiDefaultTimeout = r 10000 p nPopup "notiDefaultTimeout"
    , configDistanceTop = r 50 p nPopup "distanceTop"
    , configDistanceBetween = r 20 p nPopup "distanceBetween"
}
  where nPopup = "notification-center-notification-popup"
        nCenter = "notification-center"
        r = readConfig

getInitialState = do
  newTVarIO $ State
    { stDisplayingNotiList = []
    , stCenterShown = False}

main' :: IO ()
main' = do
  GI.init Nothing

  homeDir <- getHomeDirectory
  config <- getConfig <$> (readConfigFile
    (homeDir ++ "/.config/deadd/deadd.conf"))

  istate <- getInitialState
  notiState <- startNotificationDaemon config $ updateNotis istate
  atomically $ modifyTVar' istate $
    \istate' -> istate' { stNotiState = notiState }
  createNotiCenter istate config

  unixSignalAdd PRIORITY_HIGH (fromIntegral sigUSR1)
    (showNotiCenter istate notiState)

  GI.main

main :: IO ()
main = do
--  daemonize main'
  main'
