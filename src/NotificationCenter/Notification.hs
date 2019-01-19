{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter.Notification
  ( showNotification
  , updateNoti
  , DisplayingNotificaton(..)
  ) where

import TransparentWindow
import NotificationCenter.Notification.Glade (glade)
import NotificationCenter.Notifications
  (NotifyState(..), Notification(..))
import NotificationCenter.Notifications.Data (Urgency(..))

import Data.List
import qualified Data.Text as Text

import Control.Monad
import Control.Concurrent.STM
  ( readTVarIO, modifyTVar, TVar(..), atomically, newTVarIO )


import GI.Gtk (widgetShowAll, widgetHide, windowMove, widgetDestroy
              , labelSetText, widgetSetSizeRequest, labelSetXalign
              , widgetGetPreferredHeightForWidth, onButtonClicked
              , widgetDestroy)
import qualified GI.Gtk as Gtk
data DisplayingNotificaton = DisplayingNotificaton
  { dNotiId :: Int
  , dNotiDestroy :: IO ()
  , dLabelTitel :: Gtk.Label
  , dLabelBody :: Gtk.Label
  , dLabelAppname :: Gtk.Label
  , dLabelTime :: Gtk.Label
  , dButtonClose :: Gtk.Button
  , dContainer :: Gtk.Box
  }


instance Eq DisplayingNotificaton where
  a == b = dNotiId a == dNotiId b


showNotification :: Bool -> Gtk.Box -> DisplayingNotificaton
                 -> TVar NotifyState
                 -> (DisplayingNotificaton -> IO ())
                 -> IO DisplayingNotificaton
showNotification showFirst mainBox dNoti tNState closeNotification = do
  nState <- readTVarIO tNState
  let (Just noti) = find (\n -> notiId n == dNotiId dNoti)
        $ notiStList nState

  builder <- Gtk.builderNew
  Gtk.builderAddFromString builder (Text.pack glade) (-1)
  objs <- getObjs builder
    [ "box_container"
    , "label_titel"
    , "label_body"
    , "label_appname"
    , "label_time"
    , "img_icon"
    , "button_close"]

  labelTitel <- label objs "label_titel"
  labelBody <- label objs "label_body"
  labelAppname <- label objs "label_appname"
  labelTime <- label objs "label_time"
  buttonClose <- button objs "button_close"
  container <- box objs "box_container"

  let elemsLabel = [labelTitel, labelBody, labelAppname, labelTime]
  case (notiUrgency noti) of
    High -> do
      sequence $ (flip addClass) "critical" <$> elemsLabel
      addClass container "critical"
    Low -> do
      sequence $ (flip addClass) "low" <$> elemsLabel
      addClass container "low"
    Normal -> do
      sequence $ (flip addClass) "normal" <$> elemsLabel
      addClass container "normal"

  let dNoti' = dNoti { dNotiDestroy = widgetDestroy container
                     , dLabelTitel = labelTitel
                     , dLabelBody = labelBody
                     , dLabelAppname = labelAppname
                     , dLabelTime = labelTime
                     , dButtonClose = buttonClose
                     , dContainer = container
                     }

  Gtk.containerAdd mainBox container
  updateNoti mainBox dNoti' tNState showFirst

  onButtonClicked buttonClose $ do
    closeNotification dNoti'

  widgetShowAll container
  return dNoti'

updateNoti mainBox dNoti tNState showFirst = do
  addSource $ do
    nState <- readTVarIO tNState
    let (Just noti) = find (\n -> notiId n == dNotiId dNoti)
          $ notiStList nState
    labelSetText (dLabelTitel dNoti) $ notiSummary noti
    labelSetText (dLabelBody dNoti) $ notiBody noti
    labelSetText (dLabelAppname dNoti) $ notiAppName noti
    labelSetText (dLabelTime dNoti) $ notiTime noti
    labelSetXalign (dLabelTitel dNoti) 0
    labelSetXalign (dLabelBody dNoti) 0
    when (showFirst)
      (Gtk.boxReorderChild mainBox (dContainer dNoti) 0)
    return False
  return ()
