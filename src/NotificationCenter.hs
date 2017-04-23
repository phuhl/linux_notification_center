{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter where

import NotificationCenter.Notification
  (DisplayingNotificaton(..), showNotification)
import NotificationCenter.Notifications
  (NotifyState(..), startNotificationDaemon, Notification(..))
import NotificationCenter.Glade (glade)
import TransparentWindow

import Prelude

import Data.Maybe
import Data.IORef
import Data.List
import Data.Time
import Data.Time.LocalTime
import qualified Data.Text as Text
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
       , onButtonClicked)
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


-- constants
barHeight = 25


data State = State
  { stMainWindow :: Gtk.Window
  , stNotiBox :: Gtk.Box
  , stTimeLabel :: Gtk.Label
  , stDateLabel :: Gtk.Label
  , stDeleteAll :: Gtk.Button
  , stNotiState :: TVar NotifyState
  , stDisplayingNotiList :: [ DisplayingNotificaton ]
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


showNotiCenter :: TVar State -> IO ()
showNotiCenter state = do
  objs <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "label_time"
    , "label_date"
    , "box_notis"
    , "button_deleteAll" ]
    (Just "Notification area") Nothing

  mainWindow <- window objs "main_window"
  notiBox <- box objs "box_notis"
  timeL <- label objs "label_time"
  timeD <- label objs "label_date"
  deleteButton <- button objs "button_deleteAll"

--  (Just mainWindowGDK) <- widgetGetParentWindow mainWindow
  atomically $ modifyTVar' state $
    \state' -> state' { stMainWindow = mainWindow
                      , stNotiBox = notiBox
                      , stTimeLabel = timeL
                      , stDateLabel = timeD
                      , stDeleteAll = deleteButton }

  startSetTimeThread state


  onButtonClicked deleteButton $ do
    displayList <- stDisplayingNotiList <$> readTVarIO state
    mapM (removeNoti state) displayList
    return ()


  (screenH, screenW) <- getScreenProportions mainWindow

  onWidgetLeaveNotifyEvent mainWindow $ \(_) -> do
    widgetHide mainWindow
    return True

                                 -- w   h
  windowSetDefaultSize mainWindow 500 (screenH - barHeight)
  windowMove mainWindow (screenW - 500) barHeight
  onWidgetDestroy mainWindow mainQuit
  return ()


getInitialState = do
  newTVarIO $ State
    { stDisplayingNotiList = [] }

main :: IO ()
main = do
--  daemonize main'
  main'

main' :: IO ()
main' = do
  GI.init Nothing

  istate <- getInitialState
  notiState <- startNotificationDaemon $ updateNotis istate
  atomically $ modifyTVar' istate $
    \istate' -> istate' { stNotiState = notiState }
  showNotiCenter $ istate

  unixSignalAdd PRIORITY_HIGH (fromIntegral sigUSR1)
    (do
        mainWindow <- stMainWindow <$> readTVarIO istate
        widgetShow mainWindow
        return True)

  GI.main

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
  atomically $ modifyTVar' tState (
    \state -> state { stDisplayingNotiList =
                      newNotis' ++ stDisplayingNotiList state})
  setDeleteAllState tState
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
