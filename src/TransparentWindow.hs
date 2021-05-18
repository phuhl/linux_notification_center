{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TransparentWindow
  ( createTransparentWindow
  -- * Lookups
  , ObjDict(..)
  , gObjLookup
  , window
  , drawingArea
  , label
  , box
  , button
  , image
  , progressbar
  , adjustment
  , scale
  , getObjs
  , getScreenPos
  , getMouseActiveScreenPos
  -- * General
  , getScreenProportions
  , runAfterDelay
  , addSource
  , setStyle
  , addClass
  , removeClass
  -- * Colors
  ) where

import Data.Int ( Int32 )
import Data.Word ( Word32 )
import Data.Maybe
import Data.List (elem)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent (forkIO, threadDelay, ThreadId(..), killThread)

import GI.Gtk
       (styleContextRemoveClass, widgetShowAll, widgetHide
       , onWidgetDestroy, windowSetDefaultSize
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
       , onWidgetRealize, styleContextAddProviderForScreen
       , cssProviderLoadFromData, cssProviderNew, styleContextAddClass
       , widgetGetStyleContext, CssProvider(..))
import qualified GI.Gtk as Gtk
  (ProgressBar(..), Scale(..), DrawingArea(..), unsafeCastTo, Window(..), IsWidget(..)
  , builderGetObject, builderAddFromString
  , builderNew, Builder(..), Label(..), Box(..), Button(..), Image(..), Adjustment(..))
import GI.Gtk.Constants
import GI.Gdk (getRectangleHeight, getRectangleWidth, getRectangleY
              , getRectangleX, Monitor, monitorGetGeometry, displayGetMonitor
              , screenGetDisplay, screenGetHeight, screenGetWidth, Screen (..)
              , displayGetPointer, displayGetDefault, displayGetDefaultSeat
              , deviceGetPosition, seatGetPointer, displayGetMonitorAtPoint)

import GI.GObject.Objects (IsObject(..), Object(..))

import GI.GLib (idleSourceNew, sourceSetCallback, sourceAttach
               , sourceUnref, idleAdd, )
import GI.GLib.Constants
import GI.Cairo ()
import Foreign.Ptr (castPtr)
import qualified GHC.Int (Int32(..))


type ObjDict = [(Text.Text, GI.GObject.Objects.Object)]

gObjLookup :: (GI.GObject.Objects.Object -> IO a)
  -> ObjDict -> Text.Text -> IO a
gObjLookup f dict name = f $ fromJust $ lookup name dict

--window :: [(Text.Text, GI.GObject.Objects.Object)] -> Text.Text -> IO Gtk.Window
window = gObjLookup (Gtk.unsafeCastTo Gtk.Window)
drawingArea = gObjLookup (Gtk.unsafeCastTo Gtk.DrawingArea)
label = gObjLookup (Gtk.unsafeCastTo Gtk.Label)
adjustment = gObjLookup (Gtk.unsafeCastTo Gtk.Adjustment)
box = gObjLookup (Gtk.unsafeCastTo Gtk.Box)
button = gObjLookup (Gtk.unsafeCastTo Gtk.Button)
image  = gObjLookup (Gtk.unsafeCastTo Gtk.Image)
progressbar = gObjLookup (Gtk.unsafeCastTo Gtk.ProgressBar)
scale= gObjLookup (Gtk.unsafeCastTo Gtk.Scale)


getObjs :: Gtk.Builder -> [Text.Text] -> IO ObjDict
getObjs builder dict = do
  mapM getObj dict
  where getObj name = do
          (Just obj) <- Gtk.builderGetObject builder name
          return (name, obj)

getScreenProportions :: Gtk.Window -> IO (GHC.Int.Int32, GHC.Int.Int32)
getScreenProportions window = do
  screen <- window `get` #screen
  h <- screenGetHeight screen
  w <- screenGetWidth screen
  return (h, w)

createTransparentWindow :: Text.Text -> [Text.Text] -> Maybe Text.Text
                        -> IO (ObjDict, Gtk.Builder)
createTransparentWindow
  glade
-- ^ Content of glade-file
  objsToGet
-- ^ List of widgets that should be returned, listed by name
  title
-- ^ Optional, title of the window
  = do
  builder <- Gtk.builderNew
  Gtk.builderAddFromString builder glade (-1)
  let objsToGet' = objsToGet ++
        (filter (\name -> not $ elem name objsToGet) ["main_window"])
  objs <- getObjs builder objsToGet'

  mainWindow <- window objs "main_window"

  screen <- mainWindow `get` #screen
  visual <- #getRgbaVisual screen
  #setVisual mainWindow visual

  when (title /= Nothing) $ let (Just title') = title in
    setWindowTitle mainWindow title'

  return (objs, builder)

runAfterDelay :: Int -> IO () -> IO ThreadId
runAfterDelay t f = forkIO (threadDelay t >> f)


addSource :: IO Bool -> IO Word32
addSource f = do
  idleAdd PRIORITY_DEFAULT f


setStyle :: Screen -> BS.ByteString -> IO ()
setStyle screen style = do
  provider <- cssProviderNew
  cssProviderLoadFromData provider style
  styleContextAddProviderForScreen screen provider
    $ fromIntegral STYLE_PROVIDER_PRIORITY_USER
  return ()

addClass :: Gtk.IsWidget a => a -> Text.Text -> IO ()
addClass w clazz = do
  context <- widgetGetStyleContext w
  styleContextAddClass context clazz

removeClass :: Gtk.IsWidget a => a -> Text.Text -> IO ()
removeClass w clazz = do
  context <- widgetGetStyleContext w
  styleContextRemoveClass context clazz

getScreenPos :: Gtk.Window -> GHC.Int.Int32
  -> IO (GHC.Int.Int32, GHC.Int.Int32, GHC.Int.Int32)
getScreenPos window number = do
  screen <- window `get` #screen
  display <- screenGetDisplay screen
  monitor <- fromMaybe (error "Unknown screen")
    <$> displayGetMonitor display number
  getMonitorProps monitor

getMouseActiveScreenPos :: Gtk.Window -> GHC.Int.Int32
  -> IO (GHC.Int.Int32, GHC.Int.Int32, GHC.Int.Int32)
getMouseActiveScreenPos window number = do
  screen <- window `get` #screen
  display <- screenGetDisplay screen
  mPointerPos <- getPointerPos
  monitor <- case mPointerPos of
               Just (x, y) -> displayGetMonitorAtPoint display x y
               Nothing -> fromMaybe (error "Unknown screen")
                          <$> displayGetMonitor display number
  getMonitorProps monitor

getMonitorProps :: Monitor -> IO (GHC.Int.Int32, GHC.Int.Int32, GHC.Int.Int32)
getMonitorProps monitor = do
  monitorGeometry <- monitorGetGeometry monitor
  monitorX <- getRectangleX monitorGeometry
  monitorY <- getRectangleY monitorGeometry
  monitorWidth <- getRectangleWidth monitorGeometry
  monitorHeight <- getRectangleHeight monitorGeometry
  return (monitorX + monitorWidth, monitorY, monitorHeight)


getPointerPos :: IO (Maybe (Int32, Int32))
getPointerPos = do
  mDisplay <- displayGetDefault
  mSeat <- sequence $ displayGetDefaultSeat <$> mDisplay
  case mSeat of
    Just (seat) -> do
      mPointer <- seatGetPointer seat
      case mPointer of
        Just (pointer) -> do
          (screen, x, y) <- deviceGetPosition pointer
          return $ Just (x, y)
        Nothing -> return Nothing
    Nothing -> return Nothing
