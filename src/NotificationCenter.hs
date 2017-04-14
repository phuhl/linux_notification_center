{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter where

import NotificationCenter.Notifications (startNotificationDaemon)
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
import Control.Concurrent.STM (readTVarIO, modifyTVar, TVar(..), atomically
                              , newTVarIO)


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
import qualified GI.Gtk as Gtk (Window(..))
--import GI.GObject.Objects (IsObject(..), Object(..))

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


data State = State {
  stMainWindow :: TVar (Maybe Gtk.Window)
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


runAfterDelay :: Int -> IO () -> IO ThreadId
runAfterDelay t f = forkIO (threadDelay t >> f)

showNotiCenter :: State -> IO ()
showNotiCenter state = do
  objs <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "label_time"
    , "label_date"]
    (Just "Notification area")

  mainWindow <- window objs "main_window"

--  (Just mainWindowGDK) <- widgetGetParentWindow mainWindow
  atomically $ modifyTVar (stMainWindow state) (\_ -> Just mainWindow)

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
  windowVar <- newTVarIO Nothing
  return $ State {
    stMainWindow = windowVar
                 }

main :: IO ()
main = do
  daemonize main'

main' :: IO ()
main' = do
  GI.init Nothing

  istate <- getInitialState
  showNotiCenter istate

  unixSignalAdd PRIORITY_HIGH (fromIntegral sigUSR1)
    (do
        (Just mainWindow) <- readTVarIO $ stMainWindow istate
        widgetShowAll mainWindow
        return True)

  startNotificationDaemon

  GI.main
