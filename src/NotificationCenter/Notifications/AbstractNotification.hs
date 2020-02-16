{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter.Notifications.AbstractNotification
  ( createNotification
  , setUrgencyLevel
  , updateNotiContent
  , HasDisplayingNotificationContent(..)
  , DisplayingNotificationContent(..)
  ) where

import Helpers (markupify)
import Config (Config(..))
import NotificationCenter.Notifications.Data
  (Urgency(..), CloseType(..), Notification(..))
import NotificationCenter.Notifications.Action
  (Action(..), createAction)
import TransparentWindow (label, image, box, getObjs, addClass)

import Data.Text as Text
import Data.Int ( Int32 )

import Control.Lens.TH (makeClassy)
import Control.Lens (view, set)

import GI.Gtk (widgetShowAll, widgetHide, windowMove, widgetDestroy
              , labelSetText, labelSetMarkup, widgetSetSizeRequest
              , labelSetXalign, widgetGetPreferredHeightForWidth
              , onWidgetButtonPressEvent, imageSetFromPixbuf
              , imageSetFromIconName, setWidgetWidthRequest)
import GI.GdkPixbuf (Pixbuf(..), pixbufNewFromFileAtScale)
import qualified GI.Gtk as Gtk
  (IsWidget, Box(..), Label(..), Button(..), Window(..), Image(..)
  , Builder(..), containerAdd, containerRemove, containerGetChildren)

data DisplayingNotificationContent = DisplayingNotificationContent
  { _dLabelTitel :: Gtk.Label
  , _dLabelBody :: Gtk.Label
  , _dLabelAppname :: Gtk.Label
  , _dImgAppIcon :: Gtk.Image
  , _dContainer :: Gtk.Box
  , _dActions :: Gtk.Box
  }
makeClassy ''DisplayingNotificationContent



createNotification :: HasDisplayingNotificationContent dn =>
  Config -> Gtk.Builder -> Notification -> dn -> IO dn
createNotification config builder noti dispNoti = do

  objs <- getObjs builder [ "label_titel"
                          , "label_body"
                          , "label_appname"
                          , "img_icon"
                          , "box_container"
                          , "box_actions"]

  labelTitel <- label objs "label_titel"
  labelBody <- label objs "label_body"
  labelAppname <- label objs "label_appname"
  container <- box objs "box_container"
  actions <- box objs "box_actions"
  imgAppIcon <- image objs "img_icon"

  return
    $ set dLabelTitel labelTitel

    $ set dLabelBody labelBody
    $ set dLabelAppname labelAppname
    $ set dImgAppIcon imgAppIcon
    $ set dContainer container
    $ set dActions actions
    dispNoti

setUrgencyLevel :: Gtk.IsWidget widget => Urgency -> [widget] -> IO ()
setUrgencyLevel urgency elems = do
  case urgency of
    High -> do
      sequence $ (flip addClass) "critical" <$> elems
    Low -> do
      sequence $ (flip addClass) "low" <$> elems
    Normal -> do
      sequence $ (flip addClass) "normal" <$> elems
  return ()


updateNotiContent :: HasDisplayingNotificationContent dn
  => Config -> Notification -> dn -> IO ()
updateNotiContent config noti dNoti = do
  labelSetText (view dLabelTitel dNoti) $ notiSummary noti
  if (configNotiMarkup config) then do
      labelSetMarkup (view dLabelBody dNoti) $ markupify $ notiBody noti
      else do
      labelSetText (view dLabelBody dNoti) $ notiBody noti
  labelSetText (view dLabelAppname dNoti) $ notiAppName noti
  labelSetXalign (view dLabelTitel dNoti) 0
  labelSetXalign (view dLabelBody dNoti) 0
  if (Text.isPrefixOf "file://" $ notiIcon noti) then do
    pb <- pixbufNewFromFileAtScale
      (Text.unpack $ Text.drop 6 $ notiIcon noti)
      15 15 True
    imageSetFromPixbuf (view dImgAppIcon dNoti) (Just pb)
    else do
    imageSetFromIconName (view dImgAppIcon dNoti)
      (Just $ notiIcon noti) 10

  let takeTwo (a:b:cs) = (a,b):(takeTwo cs)
      takeTwo _ = []
  actionButtons <- sequence $ (
    \(a, b) -> createAction config (notiOnAction noti) 20 20 a b)
                   <$> takeTwo (unpack <$> notiActions noti)
  currentButtons <- Gtk.containerGetChildren (view dActions dNoti)
  sequence $ Gtk.containerRemove (view dActions dNoti) <$> currentButtons
  sequence $ Gtk.containerAdd (view dActions dNoti) <$> actionButton <$> actionButtons

  widgetShowAll (view dActions dNoti)

  return ()
