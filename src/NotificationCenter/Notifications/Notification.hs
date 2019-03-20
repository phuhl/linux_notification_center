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
  (Urgency(..), CloseType(..), Config(..), Notification(..))
import NotificationCenter.Notifications.Notification.Glade (glade)
import NotificationCenter.Notifications.Action
  (Action(..), createAction)

import Data.Text as Text
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import qualified Data.Map as Map ( Map )
import Data.List ( sortOn )

import Control.Monad

import GI.Gtk (widgetShowAll, widgetHide, windowMove, widgetDestroy
              , labelSetText, labelSetMarkup, widgetSetSizeRequest
              , labelSetXalign, widgetGetPreferredHeightForWidth
              , onWidgetButtonPressEvent
              , setWidgetWidthRequest)
import qualified GI.Gtk as Gtk
  (IsWidget, Box(..), Label(..), Button(..), Window(..)
  , containerAdd, containerRemove, containerGetChildren)

import DBus ( Variant (..) )


instance Eq Notification where
  a == b = notiId a == notiId b


data DisplayingNotificaton = DisplayingNotificaton
  { dNotiGetHeight :: IO Int32
  , dNotiTop :: Int32
  , dNotiId :: Int
  , dNotiDestroy :: IO ()
  , dMainWindow :: Gtk.Window
  , dLabelTitel :: Gtk.Label
  , dLabelBody :: Gtk.Label
  , dLabelAppname :: Gtk.Label
  , dLabelBG :: Gtk.Label
  , dContainer :: Gtk.Box
  , dActions :: Gtk.Box
  }

showNotificationWindow :: Config -> Notification
  -> [DisplayingNotificaton] -> (IO ()) -> IO DisplayingNotificaton
showNotificationWindow config noti dispNotis onClose = do

  let distanceTop = configDistanceTop config
      distanceBetween = configDistanceBetween config

  objs <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "container_box"
    , "label_titel"
    , "label_body"
    , "label_appname"
    , "label_background"
    , "action_box" ]
    Nothing

  mainWindow <- window objs "main_window"

  labelTitel <- label objs "label_titel"
  labelBody <- label objs "label_body"
  labelAppname <- label objs "label_appname"
  labelBG <- label objs "label_background"
  container <- box objs "container_box"
  actions <- box objs "action_box"

  setWidgetWidthRequest mainWindow $ fromIntegral $ configWidthNoti config
  let elemsLabel = [labelTitel, labelBody, labelAppname]
  case (notiUrgency noti) of
    High -> do
      sequence $ (flip addClass) "critical" <$> elemsLabel
      addClass mainWindow "critical"
    Low -> do
      sequence $ (flip addClass) "low" <$> elemsLabel
      addClass mainWindow "low"
    Normal -> do
      sequence $ (flip addClass) "normal" <$> elemsLabel
      addClass mainWindow "normal"

  let dNoti = DisplayingNotificaton
        { dNotiGetHeight = (getHeight container config)
        , dNotiId = notiId noti
        , dNotiDestroy = widgetDestroy mainWindow
        , dMainWindow = mainWindow
        , dLabelTitel = labelTitel
        , dLabelBody = labelBody
        , dLabelAppname = labelAppname
        , dLabelBG = labelBG
        , dContainer = container
        , dActions = actions
        }

  height <- updateNoti' config onClose noti dNoti

  --  (screenH, screenW) <- getScreenProportions mainWindow
  (screenW, screenY, screenH) <- getScreenPos mainWindow
    (fromIntegral $ configNotiMonitor config)

  hBefores <- sortOn fst <$> mapM
    (\n -> (,) (dNotiTop n) <$> (dNotiGetHeight n)) dispNotis
  let hBefore = findBefore hBefores ((fromIntegral distanceTop) + screenY)
                height (fromIntegral distanceBetween)

  windowMove mainWindow
    (screenW - fromIntegral
     (configWidthNoti config + configDistanceRight config))
    hBefore

  onWidgetButtonPressEvent mainWindow $ \(_) -> do
    notiOnClosed noti $ User
    onClose
    return False
  widgetShowAll mainWindow

  return $ dNoti { dNotiTop = hBefore }

updateNoti' config onClose noti dNoti  = do
  labelSetText (dLabelTitel dNoti) $ notiSummary noti
  if (configNotiMarkup config) then do
      labelSetMarkup (dLabelBody dNoti) $ notiBody noti
      else do
      labelSetText (dLabelBody dNoti) $ notiBody noti
  labelSetText (dLabelAppname dNoti) $ notiAppName noti
  labelSetXalign (dLabelTitel dNoti) 0
  labelSetXalign (dLabelBody dNoti) 0
  let takeTwo (a:b:cs) = (a,b):(takeTwo cs)
      takeTwo _ = []
  actionButtons <- sequence $ (
    \(a, b) -> createAction config (notiOnAction noti) 20 20 a b)
                   <$> takeTwo (unpack <$> notiActions noti)
  currentButtons <- Gtk.containerGetChildren (dActions dNoti)
  sequence $ Gtk.containerRemove (dActions dNoti) <$> currentButtons
  sequence $ Gtk.containerAdd (dActions dNoti) <$> actionButton <$> actionButtons
  widgetShowAll (dActions dNoti)
  height <- getHeight (dContainer dNoti) config
  widgetSetSizeRequest (dLabelBG dNoti) (-1) height
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
