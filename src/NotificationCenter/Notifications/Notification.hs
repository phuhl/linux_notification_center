{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter.Notifications.Notification
  ( showNotificationWindow
  , Notification(..)
  , DisplayingNotificaton(..)
  ) where

import TransparentWindow
import NotificationCenter.Notifications.Data (Urgency(..))
import NotificationCenter.Notifications.Notification.Glade (glade)

import Data.Text as Text
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import qualified Data.Map as Map ( Map )
import Data.List ( sortOn )

import Control.Monad

import GI.Gtk (widgetShowAll, widgetHide, windowMove, widgetDestroy
              , labelSetText, widgetSetSizeRequest, labelSetXalign
              , widgetGetPreferredHeightForWidth)

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
  }

-- constants
notiDefaultTimeout = 10000
distanceTop = 50
distanceBetween = 20

showNotificationWindow :: Notification -> [DisplayingNotificaton]
  -> (IO ()) -> IO DisplayingNotificaton
showNotificationWindow noti dispNotis onClose = do

  objs <- createTransparentWindow (Text.pack glade)
    ["main_window"
    , "container_box"
    , "main_bg"
    , "label_titel"
    , "label_body"
    , "label_appname"]
    Nothing

  mainWindow <- window objs "main_window"

  labelTitel <- label objs "label_titel"
  labelBody <- label objs "label_body"
  labelAppname <- label objs "label_appname"
  container <- box objs "container_box"
  background <- drawingArea objs "main_bg"

  labelSetText labelTitel $ notiSummary noti
  labelSetText labelBody $ notiBody noti
  labelSetText labelAppname $ notiAppName noti
  labelSetXalign labelTitel 0
  labelSetXalign labelBody 0

  case (notiUrgency noti) of
    High -> addClass mainWindow "critical"
    Low -> addClass mainWindow "low"
    Normal -> addClass mainWindow "normal"

  height <- getHeight container
  widgetSetSizeRequest background (-1) height

  hBefores <- sortOn fst <$> mapM
    (\n -> (,) (dNotiTop n) <$> (dNotiGetHeight n)) dispNotis

  let hBefore = findBefore hBefores distanceTop height

  (screenH, screenW) <- getScreenProportions mainWindow
  windowMove mainWindow (screenW - 350) hBefore

  startTimeoutThread objs (fromIntegral $ notiTimeout noti) onClose

  widgetShowAll mainWindow

  return $ DisplayingNotificaton
    { dNotiGetHeight = (getHeight container)
    , dNotiId = notiId noti
    , dNotiTop = hBefore
    , dNotiDestroy = widgetDestroy mainWindow
    }

getHeight widget = do
  (a, b) <- widgetGetPreferredHeightForWidth widget 300
  return a

startTimeoutThread objs timeout onClose = do
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


findBefore ((s, l):bs) p height =
  if ((p + height) <= (s - distanceBetween)) then
    p
  else
    findBefore bs (s + l + distanceBetween) height
findBefore [] p _ = p
