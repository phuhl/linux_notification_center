{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.List ( sortOn, filter )
import Data.Maybe ( fromMaybe, isJust )

import Control.Monad
import DBus ( Variant (..) )

import GI.Gdk (getEventButtonButton)
import GI.Gtk (widgetGetPreferredHeightForWidth, widgetSetSizeRequest
              , widgetShowAll, onWidgetButtonPressEvent, windowMove
              , setWidgetWidthRequest, widgetDestroy, labelSetLines, labelSetEllipsize)
import GI.Pango.Enums (EllipsizeMode(..))
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
  , _dHasCustomPosition :: Bool
  }
makeClassy ''DisplayingNotificationPopup
instance HasDisplayingNotificationContent DisplayingNotificationPopup where
  displayingNotificationContent = dpopupContent


showNotificationWindow :: Config -> Notification
  -> [DisplayingNotificationPopup] -> (IO ()) -> IO DisplayingNotificationPopup
showNotificationWindow config noti dispNotis onClose = do

  let distanceTopFromConfig = configDistanceTop config
      distanceTop = fromIntegral $ fromMaybe distanceTopFromConfig (notiTop noti)
      distanceBetween = configDistanceBetween config
      distanceRight = fromMaybe (configDistanceRight config) (notiRight noti)
      hasCustomPosition = (isJust $ notiTop noti) || (isJust $ notiRight noti)

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
    , _dHasCustomPosition = hasCustomPosition
    , _dpopupContent = DisplayingNotificationContent {} }


  let dispNoti = set dNotiGetHeight
        (getHeight (view dContainer dispNotiWithoutHeight) config)
        dispNotiWithoutHeight
      lblBody = (flip view) dispNoti $ dLabelBody

  labelSetEllipsize lblBody
    (if configPopupEllipsizeBody config
     then EllipsizeModeEnd
     else EllipsizeModeNone)

  labelSetLines lblBody $ fromIntegral $ configPopupMaxLinesInBody config

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
    (\n -> (,) (_dNotiTop n) <$> (_dNotiGetHeight n)) (Data.List.filter (not . _dHasCustomPosition) dispNotis)
  let hBefore = if hasCustomPosition then
                  distanceTop
                else
                  findBefore hBefores (distanceTop + screenY)
                  height (fromIntegral distanceBetween)

  windowMove mainWindow
    (screenW - fromIntegral
     (configWidthNoti config + distanceRight))
    hBefore

  onWidgetButtonPressEvent mainWindow $ \eventButton -> do
    mouseButton <- (\n -> "mouse" ++ n) . show <$> getEventButtonButton eventButton
    let validMouseButtons = ["mouse1", "mouse2", "mouse3", "mouse4", "mouse5"]
        validInput = mouseButton `elem` validMouseButtons
        validDismiss = configPopupDismissButton config `elem` validMouseButtons
        validDefaultAction = configPopupDefaultActionButton config `elem` validMouseButtons
        valid = validInput && validDismiss && validDefaultAction
        dismiss = configPopupDismissButton config == mouseButton
        defaultAction = configPopupDefaultActionButton config == mouseButton
    if | valid && dismiss -> do
           notiOnClosed noti $ User
           onClose
       | valid && defaultAction -> do 
           notiOnAction noti "default"
           notiOnClosed noti $ User
           onClose
       | not validDismiss -> do
           putStrLn $ "Warning: Unknown mouse button '" ++ (show $ configPopupDismissButton config) ++ "'."
           notiOnClosed noti $ User
           onClose
       | not validDefaultAction -> do
           putStrLn $ "Warning: Unknown mouse button '" ++ (show $ configPopupDefaultActionButton config) ++ "'."
           notiOnClosed noti $ User
           onClose
       | not validInput -> do
           putStrLn $ "Warning: Popup received unknown mouse input '" ++ (show mouseButton) ++ "'."
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
