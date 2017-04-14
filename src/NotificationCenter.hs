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
  ( readTVarIO, modifyTVar, TVar(..), atomically, newTVarIO )

import System.Locale.Read
import System.Posix.Signals (sigUSR1)
import System.Posix.Daemonize (serviced, daemonize)

import GI.Gtk
       (widgetShowAll, widgetHide, onWidgetDestroy, windowSetDefaultSize
       , setWindowTitle, boxPackStart, boxNew, setWindowWindowPosition
       , WindowPosition(..), windowMove
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
       , onWidgetRealize)
import qualified GI.Gtk as Gtk (Window(..), Box(..))
--import GI.GObject.Objects (IsObject(..), Object(..))

import qualified GI.Gtk as GI (init, main)
import GI.GLib (sourceRemove, timeoutAdd, unixSignalAdd)
import GI.GLib.Constants
import GI.Gdk (threadsInit, threadsEnter, threadsLeave)
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
  , stNotiState :: TVar NotifyState
  , stDisplayingNotiList :: [ DisplayingNotificaton ]
  }


setTime objs = do
  timeL <- label objs "label_time"
  timeD <- label objs "label_date"
  now <- zonedTimeToLocalTime <$> getZonedTime
  zone <- System.Locale.Read.getCurrentLocale
  let format = Text.pack . flip (formatTime zone) now
  labelSetText timeL $ format "%H:%M"
  labelSetText timeD $ format "%A, %x"

startSetTimeThread :: ObjDict -> IO ()
startSetTimeThread objs = do
  setTime objs
  time <- fromIntegral <$> diffTimeToPicoseconds <$> utctDayTime <$> getCurrentTime
  let delay = (60 * 1000000) - ((ceiling (time / 1000000)) `mod` (1000000 * 60))
  runAfterDelay delay (startSetTimeThread objs)
  return ()


showNotiCenter :: TVar State -> IO ()
showNotiCenter state = do
  objs <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "label_time"
    , "label_date"
    , "box_notis"]
    (Just "Notification area") Nothing

  mainWindow <- window objs "main_window"
  notiBox <- box objs "box_notis"

--  (Just mainWindowGDK) <- widgetGetParentWindow mainWindow
  atomically $ modifyTVar state $
    \state' -> state' { stMainWindow = mainWindow
                      , stNotiBox = notiBox}

  startSetTimeThread objs


--  (Just display) <- displayGetDefault
--  seat <- displayGetDefaultSeat display
--    GrabStatusSuccess <- seatGrab seat mainWindowGDK [SeatCapabilitiesPointer] True
--      Nothing Nothing Nothing


--  monitor <- displayGetMonitorAtWindow display mainWindowGDK
--  rect <- monitorGetGeometry monitor
--  screenH <- getRectangleWidth rect

  (screenH, screenW) <- getScreenProportions mainWindow

  onWidgetLeaveNotifyEvent mainWindow $ \(_) -> do
--      mainQuit
    widgetHide mainWindow
    return True

--  onWidgetRealize mainWindow $ do
--    startNotificationDaemon
--    return ()

                                 -- w   h
  windowSetDefaultSize mainWindow 500 (screenH - barHeight)
  windowMove mainWindow (screenW - 500) barHeight
  onWidgetDestroy mainWindow mainQuit
--  widgetShowAll mainWindow
  return ()


getInitialState = do
  newTVarIO $ State
    { stDisplayingNotiList = [] }

main :: IO ()
main = do
  daemonize main'

main' :: IO ()
main' = do
  GI.init Nothing

  threadsInit

  threadsEnter
  istate <- getInitialState
  notiState <- startNotificationDaemon $ updateNotis istate
  atomically $ modifyTVar istate $
    \istate' -> istate' { stNotiState = notiState }
  showNotiCenter $ istate

  unixSignalAdd PRIORITY_HIGH (fromIntegral sigUSR1)
    (do
        mainWindow <- stMainWindow <$> readTVarIO istate
        widgetShowAll mainWindow
        return True)

  GI.main
  threadsLeave

updateNotis :: TVar State -> IO()
updateNotis tState = do
  threadsEnter
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
        $ stNotiState state
      return $ newNoti
    ) newNotis
  atomically $ modifyTVar tState (
    \state -> state { stDisplayingNotiList =
                      newNotis' ++ stDisplayingNotiList state})
  threadsLeave
  return ()
