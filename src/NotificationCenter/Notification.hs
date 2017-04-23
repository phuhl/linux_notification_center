{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter.Notification
  ( showNotification
  , DisplayingNotificaton(..)
  ) where

import TransparentWindow
import NotificationCenter.Notification.Glade (glade)
import NotificationCenter.Notifications
  (NotifyState(..), Notification(..))

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
  (Box(..), builderGetObject
  , builderAddFromString , builderNew, Builder(..), containerAdd)

data DisplayingNotificaton = DisplayingNotificaton
  { dNotiId :: Int
  , dNotiDestroy :: IO ()
  }

instance Eq DisplayingNotificaton where
  a == b = dNotiId a == dNotiId b


showNotification :: Gtk.Box -> DisplayingNotificaton
                 -> TVar NotifyState
                 -> (DisplayingNotificaton -> IO ())
                 -> IO DisplayingNotificaton
showNotification mainBox dNoti tNState closeNotification = do
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

  let dNoti' = dNoti { dNotiDestroy = widgetDestroy container }

  labelSetText labelTitel $ notiSummary noti
  labelSetText labelBody $ notiBody noti
  labelSetText labelAppname $ notiAppName noti
  labelSetText labelTime $ notiTime noti
  labelSetXalign labelTitel 0
  labelSetXalign labelBody 0

  onButtonClicked buttonClose $ do
    closeNotification dNoti'

  Gtk.containerAdd mainBox container

  widgetShowAll container
  return dNoti'
