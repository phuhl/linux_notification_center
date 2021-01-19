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

import Helpers ((=<<?))

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

import qualified GI.Gtk as Gtk

import GI.Gtk.Constants
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk

import GI.GObject.Objects (IsObject(..), Object(..))

import GI.GLib (idleSourceNew, sourceSetCallback, sourceAttach
               , sourceUnref, idleAdd, )
import GI.GLib.Constants
import GI.Cairo ()
import Graphics.Rendering.Cairo
       (fill, restore, save, stroke, arc, setDash, setLineWidth, rotate
       , rectangle, setSourceRGBA, setSourceRGB, newPath, translate
       , lineTo, moveTo, Render)
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Graphics.Rendering.Cairo.Internal (Render(..))
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
  monitor <- getMonitor window 0
  geometry <- Gdk.monitorGetGeometry monitor
  h <- Gdk.getRectangleHeight geometry
  w <- Gdk.getRectangleWidth geometry
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

--  screen <- mainWindow `get` #screen
--  visual <- #getRgbaVisual screen
--  #setVisual mainWindow visual

  when (title /= Nothing) $ let (Just title') = title in
    Gtk.setWindowTitle mainWindow title'

  return (objs, builder)

runAfterDelay :: Int -> IO () -> IO ThreadId
runAfterDelay t f = forkIO (threadDelay t >> f)


addSource :: IO Bool -> IO Word32
addSource f = do
  idleAdd PRIORITY_DEFAULT f


setStyle :: Gdk.Display -> BS.ByteString -> IO ()
setStyle screen style = do
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData provider style
  Gtk.styleContextAddProviderForDisplay screen provider
    $ fromIntegral STYLE_PROVIDER_PRIORITY_USER
  return ()

addClass :: Gtk.IsWidget a => a -> Text.Text -> IO ()
addClass w clazz = do
  context <- Gtk.widgetGetStyleContext w
  Gtk.styleContextAddClass context clazz

removeClass :: Gtk.IsWidget a => a -> Text.Text -> IO ()
removeClass w clazz = do
  context <- Gtk.widgetGetStyleContext w
  Gtk.styleContextRemoveClass context clazz

getScreenPos :: Gtk.Window -> GHC.Int.Int32
  -> IO (GHC.Int.Int32, GHC.Int.Int32, GHC.Int.Int32)
getScreenPos window number =
  getMonitorProps =<< getMonitor window number

getDisplay :: Gtk.Window -> IO Gdk.Display
getDisplay window = do
  display <- window `Gdk.get` #display :: IO (Maybe Gdk.Display)
  return $ fromMaybe (error "No display found") display

getMonitor :: Gtk.Window -> GHC.Int.Int32 -> IO Gdk.Monitor
getMonitor window number = do
  display <- window `Gdk.get` #display :: IO (Maybe Gdk.Display)
  monitors <- sequence $ Gdk.displayGetMonitors <$> display :: IO (Maybe Gio.ListModel)
  monitorObj <- (flip Gio.listModelGetItem) 0 =<<? monitors :: IO (Maybe GI.GObject.Objects.Object)
  monitor <- sequence $ (Gtk.unsafeCastTo Gdk.Monitor) <$> monitorObj
  return $ fromMaybe (error "Unknown screen") monitor

getMouseActiveScreenPos :: Gtk.Window -> GHC.Int.Int32
  -> IO (GHC.Int.Int32, GHC.Int.Int32, GHC.Int.Int32)
getMouseActiveScreenPos window number = do
  display <- getDisplay window
  mPointerPos <- getPointerPos
  monitor <- case mPointerPos of
               Just (surface) -> Gdk.displayGetMonitorAtSurface display surface
               Nothing -> getMonitor window number
  getMonitorProps monitor

getMonitorProps :: Gdk.Monitor -> IO (GHC.Int.Int32, GHC.Int.Int32, GHC.Int.Int32)
getMonitorProps monitor = do
  geometry <- Gdk.monitorGetGeometry monitor
  h <- Gdk.getRectangleHeight geometry
  w <- Gdk.getRectangleWidth geometry
  x <- Gdk.getRectangleX geometry
  y <- Gdk.getRectangleY geometry
  return (x + w, y, h)


getPointerPos :: IO (Maybe (Gdk.Surface))
getPointerPos = do
  mDisplay <- Gdk.displayGetDefault
  mSeat <- Gdk.displayGetDefaultSeat =<<? mDisplay
  mPointer <- Gdk.seatGetPointer =<<? mSeat
  case mPointer of
    Just (pointer) -> do
      (msurface, x, y) <- Gdk.deviceGetSurfaceAtPosition pointer
      return msurface
    Nothing -> return Nothing
