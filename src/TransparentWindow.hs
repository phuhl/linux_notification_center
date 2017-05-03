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
  , getObjs
  -- * General
  , getScreenProportions
  , runAfterDelay
  , addSource
  , setStyle
  , addClass
  -- * Colors
  ) where

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
       , onWidgetRealize, styleContextAddProviderForScreen
       , cssProviderLoadFromData, cssProviderNew, styleContextAddClass
       , widgetGetStyleContext)
import qualified GI.Gtk as Gtk
  (DrawingArea(..), unsafeCastTo, Window(..), IsWidget(..)
  , builderGetObject, builderAddFromString
  , builderNew, Builder(..), Label(..), Box(..), Button(..))
import GI.Gtk.Constants
import GI.Gdk (screenGetHeight, screenGetWidth, Screen (..))
import GI.GObject.Objects (IsObject(..), Object(..))

import GI.GLib (idleSourceNew, sourceSetCallback, sourceAttach
               , sourceUnref, idleAdd, )
import GI.GLib.Constants
import GI.Cairo
import Graphics.Rendering.Cairo
       (fill, restore, save, stroke, arc, setDash, setLineWidth, rotate
       , rectangle, setSourceRGBA, setSourceRGB, newPath, scale, translate
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
box = gObjLookup (Gtk.unsafeCastTo Gtk.Box)
button = gObjLookup (Gtk.unsafeCastTo Gtk.Button)


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
                        -> IO ObjDict
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
        (filter (\name -> not $ elem name objsToGet) ["main_window", "main_bg"])
  objs <- getObjs builder objsToGet'

  mainWindow <- window objs "main_window"
  drawingArea <- drawingArea objs "main_bg"

  screen <- mainWindow `get` #screen
  visual <- #getRgbaVisual screen
  #setVisual mainWindow visual

  when (title /= Nothing) $ let (Just title') = title in
    setWindowTitle mainWindow title'

  return objs

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
