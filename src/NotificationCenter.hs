{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter where

import Control.Applicative
import Prelude

import Data.Maybe
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
import qualified GI.Gtk as Gtk (DrawingArea(..), unsafeCastTo, Window(..)
                               , builderGetObject, builderAddFromFile
                               , builderNew, Builder(..), Label(..))
import GI.Cairo
import GI.GObject.Objects (IsObject(..), Object(..))
import Control.Monad
import Data.IORef
import Data.List
import System.Locale.Read
import Data.Time
import Data.Time.LocalTime
import qualified Data.Text as Text
import qualified Data.Text.Internal as TextI
import qualified Data.Text.Internal.Lazy as TextL
import qualified Data.Text.Internal.Builder as TextB
import Data.Complex
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.Rendering.Cairo
       (fill, restore, save, stroke, arc, setDash, setLineWidth, rotate
       , rectangle, setSourceRGBA, setSourceRGB, newPath, scale, translate
       , lineTo, moveTo, Render)
import qualified GI.Gtk as GI (init, main)
import GI.GLib (sourceRemove, timeoutAdd, unixSignalAdd)
import GI.GLib.Constants
import GI.Gdk
       (getEventMotionY, getEventMotionX, windowGetHeight
       , windowGetWidth, getEventMotionWindow, screenGetHeight, screenGetWidth
       , displayGetDefault, displayGetMonitorAtWindow, monitorGetGeometry
       , getRectangleWidth, SeatCapabilities(..)
       , GrabStatus(..), displayGetDefaultSeat, seatGrab)
import GI.Gdk.Constants
import GI.Gdk.Flags (EventMask(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ShadowType(..), PositionType(..))

import Data.Monoid ((<>))
import Data.GI.Base.BasicConversions (gflagsToWord)
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Internal (Render(..))
import qualified GI.Gdk.Objects.Window
import Control.Concurrent (forkIO, threadDelay, ThreadId(..))
import Control.Concurrent.STM
--import NotificationCenter.Notifications
import System.Posix.Signals (sigUSR1)

-- constants
barHeight = 25


data State = State {
  stMainWindow :: TVar (Maybe Gtk.Window)
                   }

getObjs :: Gtk.Builder -> [Text.Text] -> IO [(Text.Text, GI.GObject.Objects.Object)]
getObjs builder dict = do
  mapM getObj dict
  where getObj name = do
          (Just obj) <- Gtk.builderGetObject builder name
          return (name, obj)


gObjLookup :: (GI.GObject.Objects.Object -> IO a)
  -> [(Text.Text, GI.GObject.Objects.Object)] -> Text.Text -> IO a
gObjLookup f dict name = f $ fromJust $ lookup name dict

--window :: [(Text.Text, GI.GObject.Objects.Object)] -> Text.Text -> IO Gtk.Window
window = gObjLookup (Gtk.unsafeCastTo Gtk.Window)
drawingArea = gObjLookup (Gtk.unsafeCastTo Gtk.DrawingArea)
label = gObjLookup (Gtk.unsafeCastTo Gtk.Label)

setTime objs = do
  timeL <- label objs "label_time"
  timeD <- label objs "label_date"
  now <- zonedTimeToLocalTime <$> getZonedTime
  zone <- System.Locale.Read.getCurrentLocale
  let format = Text.pack . flip (formatTime zone) now
  labelSetText timeL $ format "%H:%M"
  labelSetText timeD $ format "%A, %x"

startSetTimeThread :: [(Text.Text, GI.GObject.Objects.Object)] -> IO ()
startSetTimeThread objs = do
  setTime objs
  time <- fromIntegral <$> diffTimeToPicoseconds <$> utctDayTime <$> getCurrentTime
  let delay = (60 * 1000000) - ((ceiling (time / 1000000)) `mod` (1000000 * 60))
  runAfterDelay delay (startSetTimeThread objs)
  return ()


runAfterDelay :: Int -> IO () -> IO ThreadId
runAfterDelay t f = forkIO (threadDelay t >> f)

renderBG :: Double -> Double -> Render ()
renderBG w h = do
--  save
  setSourceRGBA 0.1953125 0.203125 0 0.6640625
  rectangle 0 0 w h
  fill
--  restore

showNotiCenter :: State -> IO ()
showNotiCenter state = do
  builder <- Gtk.builderNew
  Gtk.builderAddFromFile builder "notification_center.glade"
  objs <- getObjs builder [ "main_window"
                          , "drawing_area"
                          , "label_time"
                          , "label_date"]

  mainWindow <- window objs "main_window"
  drawingArea <- drawingArea objs "drawing_area"
--  (Just mainWindowGDK) <- widgetGetParentWindow mainWindow
  atomically $ modifyTVar (stMainWindow state) (\_ -> Just mainWindow)

  startSetTimeThread objs

  screen <- mainWindow `get` #screen
  visual <- #getRgbaVisual screen
  #setVisual mainWindow visual

  (Just display) <- displayGetDefault
  seat <- displayGetDefaultSeat display
--    GrabStatusSuccess <- seatGrab seat mainWindowGDK [SeatCapabilitiesPointer] True
--      Nothing Nothing Nothing


--  monitor <- displayGetMonitorAtWindow display mainWindowGDK
--  rect <- monitorGetGeometry monitor
--  screenH <- getRectangleWidth rect


  screenH <- screenGetHeight screen
  screenW <- screenGetWidth screen

  onWidgetDraw drawingArea $ \(Context fp) -> withManagedPtr fp $ \p -> (`runReaderT` Cairo (castPtr p)) $ runRender $ do
    w <- liftIO $ fromIntegral <$> widgetGetAllocatedWidth drawingArea
    h <- liftIO $ fromIntegral <$> widgetGetAllocatedHeight drawingArea
    renderBG w h
    return True


--    onWidgetKeyReleaseEvent
  onWidgetLeaveNotifyEvent mainWindow $ \(_) -> do
--      mainQuit
    widgetHide mainWindow
    return True

  onWidgetRealize mainWindow $ do
--    startNotificationDaemon
    return ()

  setWindowTitle mainWindow "Notification area"
                                 -- w   h
  windowSetDefaultSize mainWindow 500 (screenH - barHeight)
  windowMove mainWindow (screenW - 500) barHeight
  onWidgetDestroy mainWindow mainQuit
  widgetShowAll mainWindow


getInitialState = do
  windowVar <- newTVarIO Nothing
  return $ State {
    stMainWindow = windowVar
                 }

main :: IO ()
main = do
  GI.init Nothing

  istate <- getInitialState
  showNotiCenter istate

  unixSignalAdd PRIORITY_HIGH (fromIntegral sigUSR1)
    (do
        (Just mainWindow) <- readTVarIO $ stMainWindow istate
        widgetShowAll mainWindow
        return True)

  GI.main
