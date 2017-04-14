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
              , widgetGetPreferredHeightForWidth)
import qualified GI.Gtk as Gtk
  (Box(..), builderGetObject
  , builderAddFromString , builderNew, Builder(..), containerAdd)

data DisplayingNotificaton = DisplayingNotificaton
  { dNotiId :: Int
  } deriving Eq


showNotification :: Gtk.Box -> DisplayingNotificaton
                 -> TVar NotifyState -> IO ()
showNotification mainBox dNoti tNState= do
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
    , "img_icon"]

  labelTitel <- label objs "label_titel"
  labelBody <- label objs "label_body"
  labelAppname <- label objs "label_appname"
  labelTime <- label objs "label_time"

  labelSetText labelTitel $ notiSummary noti
  labelSetText labelBody $ notiBody noti
  labelSetText labelAppname $ notiAppName noti
  labelSetXalign labelTitel 0
  labelSetXalign labelBody 0


  container <- box objs "box_container"
  Gtk.containerAdd mainBox container

  widgetShowAll container
  return ()
