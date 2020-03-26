{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter.Notifications.NotificationPopup
  ( showNotificationWindow
  , updateNoti
  , DisplayingNotificationPopup(..)
  ) where

import TransparentWindow (getMouseActiveScreenPos, addSource, runAfterDelay, getScreenPos, label
                         , window, createTransparentWindow)
import Config (Config(..))
import NotificationCenter.Notifications.AbstractNotification
  (DisplayingNotificationContent(..), HasDisplayingNotificationContent(..)
  , createNotification, updateNotiContent, setUrgencyLevel)
import NotificationCenter.Notifications.Data (CloseType(..), Notification(..))
import NotificationCenter.Notifications.Notification.Glade (glade)

import Control.Lens.TH (makeClassy)
import Control.Lens (view, set)

import Data.Text as Text
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import qualified Data.Map as Map ( Map )
import Data.List ( sortOn )

import Control.Monad
import DBus ( Variant (..) )

import GI.Gtk (widgetGetPreferredHeightForWidth, widgetSetSizeRequest
              , widgetShowAll, onWidgetButtonPressEvent, windowMove
              , setWidgetWidthRequest, widgetDestroy)
import qualified GI.Gtk as Gtk (Window(..), Label(..))


instance Eq DisplayingNotificationPopup where
  a == b = _dNotiId a == _dNotiId b



data DisplayingNotificationPopup = DisplayingNotificationPopup
  { _dpopupContent :: DisplayingNotificationContent
  , _dNotiGetHeight :: IO Int32
  , _dNotiTop :: Int32
  , _dNotiId :: Int
  , _dNotiDestroy :: IO ()
  , _dMainWindow :: Gtk.Window
  , _dLabelBG :: Gtk.Label
  }
makeClassy ''DisplayingNotificationPopup
instance HasDisplayingNotificationContent DisplayingNotificationPopup where
  displayingNotificationContent = dpopupContent

showNotificationWindow :: Config -> Notification
  -> [DisplayingNotificationPopup] -> (IO ()) -> IO DisplayingNotificationPopup
showNotificationWindow config noti dispNotis onClose = do

  let distanceTop = configDistanceTop config
      distanceBetween = configDistanceBetween config

  (objs, builder) <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "label_background" ]
    Nothing

  mainWindow <- window objs "main_window"
  labelBG <- label objs "label_background"

  dispNotiWithoutHeight <- createNotification config builder noti
    $ DisplayingNotificationPopup
    { _dMainWindow = mainWindow
    , _dLabelBG = labelBG
    , _dNotiId = notiId noti
    , _dNotiDestroy = widgetDestroy mainWindow
    , _dpopupContent = DisplayingNotificationContent {} }

  let dispNoti = set dNotiGetHeight
        (getHeight (view dContainer dispNotiWithoutHeight) config)
        dispNotiWithoutHeight

  setWidgetWidthRequest mainWindow $ fromIntegral $ configWidthNoti config

  setUrgencyLevel (notiUrgency noti) [mainWindow]
  setUrgencyLevel (notiUrgency noti)
    $ (flip view) dispNoti <$> [dLabelTitel, dLabelBody, dLabelAppname]

  height <- updateNoti' config onClose noti dispNoti

  (screenW, screenY, screenH) <- if configNotiFollowMouse config then
                                   getMouseActiveScreenPos mainWindow
                                   (fromIntegral $ configNotiMonitor config)
                                  else
                                   getScreenPos mainWindow
                                   (fromIntegral $ configNotiMonitor config)

  hBefores <- sortOn fst <$> mapM
    (\n -> (,) (_dNotiTop n) <$> (_dNotiGetHeight n)) dispNotis
  let hBefore = findBefore hBefores ((fromIntegral distanceTop) + screenY)
                height (fromIntegral distanceBetween)

  windowMove mainWindow
    (screenW - fromIntegral
     (configWidthNoti config + configDistanceRight config))
    hBefore

  onWidgetButtonPressEvent mainWindow $ \(_) -> do
    notiOnAction noti "default"
    notiOnClosed noti $ User
    onClose
    return False
  widgetShowAll mainWindow

  return $ dispNoti { _dNotiTop = hBefore }

updateNoti' :: Config -> (IO ()) -> Notification -> DisplayingNotificationPopup -> IO Int32
updateNoti' config onClose noti dNoti = do
  updateNotiContent config noti dNoti

  height <- getHeight (view dContainer dNoti) config
  widgetSetSizeRequest (_dLabelBG dNoti) (-1) height
  let notiDefaultTimeout = configNotiDefaultTimeout config
  startTimeoutThread notiDefaultTimeout
    (fromIntegral $ notiTimeout noti) (
    do addSource $ do notiOnClosed noti $ Timeout
                      return False
       onClose)
  return height

updateNoti config onClose noti dNoti  = do
  addSource $ do
    updateNoti' config onClose noti dNoti
    return False
  return ()

getHeight widget config = do
  (a, b) <- widgetGetPreferredHeightForWidth widget
    $ fromIntegral $ configWidthNoti config
  return a

startTimeoutThread notiDefaultTimeout timeout onClose = do
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
