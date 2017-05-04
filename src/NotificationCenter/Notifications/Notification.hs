{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter.Notifications.Notification
  ( showNotificationWindow
  , updateNoti
  , Notification(..)
  , DisplayingNotificaton(..)
  ) where

import TransparentWindow
import NotificationCenter.Notifications.Data
  (Urgency(..), Config(..))
import NotificationCenter.Notifications.Notification.Glade (glade)

import Data.Text as Text
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import qualified Data.Map as Map ( Map )
import Data.List ( sortOn )

import Control.Monad

import GI.Gtk (widgetShowAll, widgetHide, windowMove, widgetDestroy
              , labelSetText, widgetSetSizeRequest, labelSetXalign
              , widgetGetPreferredHeightForWidth

              , setWidgetWidthRequest)
import qualified GI.Gtk as Gtk
  (Box(..), Label(..), Button(..))

import DBus ( Variant (..) )
data Notification = Notification
  { notiAppName :: Text -- ^ Application name
  , notiRepId :: Word32 -- ^ Replaces id
  , notiId :: Int -- ^ Id
  , notiIcon :: Text -- ^ App icon
  , notiSummary :: Text -- ^ Summary
  , notiBody :: Text -- ^ Body
  , notiActions :: [Text] -- ^ Actions
  , notiHints :: Map.Map Text Variant -- ^ Hints
  , notiUrgency :: Urgency
  , notiTimeout :: Int32 -- ^ Expires timeout (milliseconds)
  , notiTime :: Text
  }

data DisplayingNotificaton = DisplayingNotificaton
  { dNotiGetHeight :: IO Int32
  , dNotiTop :: Int32
  , dNotiId :: Int
  , dNotiDestroy :: IO ()
  , dLabelTitel :: Gtk.Label
  , dLabelBody :: Gtk.Label
  , dLabelAppname :: Gtk.Label
  , dContainer :: Gtk.Box
  }

showNotificationWindow :: Config -> Notification
  -> [DisplayingNotificaton] -> (IO ()) -> IO DisplayingNotificaton
showNotificationWindow config noti dispNotis onClose = do

  let notiDefaultTimeout = configNotiDefaultTimeout config
      distanceTop = configDistanceTop config
      distanceBetween = configDistanceBetween config

  objs <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "container_box"
    , "label_titel"
    , "label_body"
    , "label_appname"]
    Nothing

  mainWindow <- window objs "main_window"

  labelTitel <- label objs "label_titel"
  labelBody <- label objs "label_body"
  labelAppname <- label objs "label_appname"
  container <- box objs "container_box"

  case (notiUrgency noti) of
    High -> addClass mainWindow "critical"
    Low -> addClass mainWindow "low"
    Normal -> addClass mainWindow "normal"

  height <- getHeight container
  widgetSetSizeRequest mainWindow (300) (-1)

  hBefores <- sortOn fst <$> mapM
    (\n -> (,) (dNotiTop n) <$> (dNotiGetHeight n)) dispNotis

  let hBefore = findBefore hBefores (fromIntegral distanceTop)
                height (fromIntegral distanceBetween)

  (screenH, screenW) <- getScreenProportions mainWindow
  windowMove mainWindow (screenW - 350) hBefore

  startTimeoutThread notiDefaultTimeout objs
    (fromIntegral $ notiTimeout noti) onClose

  let dNoti = DisplayingNotificaton
        { dNotiGetHeight = (getHeight container)
        , dNotiId = notiId noti
        , dNotiTop = hBefore
        , dNotiDestroy = widgetDestroy mainWindow
        , dLabelTitel = labelTitel
        , dLabelBody = labelBody
        , dLabelAppname = labelAppname
        , dContainer = container
        }

  updateNoti dNoti noti
  widgetShowAll mainWindow

  return dNoti

updateNoti dNoti noti = do
  addSource $ do
    labelSetText (dLabelTitel dNoti) $ notiSummary noti
    labelSetText (dLabelBody dNoti) $ notiBody noti
    labelSetText (dLabelAppname dNoti) $ notiAppName noti
    labelSetXalign (dLabelTitel dNoti) 0
    labelSetXalign (dLabelBody dNoti) 0
    return False
  return ()

getHeight widget = do
  (a, b) <- widgetGetPreferredHeightForWidth widget 300
  return a

startTimeoutThread notiDefaultTimeout objs timeout onClose = do
  when (timeout /= 0) $ do
    let timeout' = if timeout > 0 then timeout
                   else notiDefaultTimeout
    runAfterDelay (1000 * timeout') $ do
      addSource $ do
        onClose
        return False
      return ()
    return ()
  return ()


findBefore ((s, l):bs) p height distanceBetween =
  if ((p + height) <= (s - distanceBetween)) then
    p
  else
    findBefore bs (s + l + distanceBetween) height distanceBetween
findBefore [] p _ _ = p
