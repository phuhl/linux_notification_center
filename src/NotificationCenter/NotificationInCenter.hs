{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter.NotificationInCenter
  ( showNotification
  , updateNoti
  , DisplayingNotificationInCenter(..)
  ) where

import TransparentWindow
import NotificationCenter.Notification.Glade (glade)
import NotificationCenter.Notifications (NotifyState(..))
import Config (Config(..))
import NotificationCenter.Notifications.Data (Notification(..))
import NotificationCenter.Notifications.AbstractNotification
  (DisplayingNotificationContent(..), HasDisplayingNotificationContent(..)
  , createNotification, updateNotiContent, setUrgencyLevel)

import Data.List
import qualified Data.Text as Text

import Control.Lens.TH (makeClassy)
import Control.Lens (view, set)


import Control.Monad
import Control.Concurrent.STM
  ( readTVarIO, modifyTVar, TVar(..), atomically, newTVarIO )

import GI.Pango.Enums (EllipsizeMode(..))
import qualified GI.Gtk as Gtk
  (widgetShowAll, onButtonClicked, boxReorderChild, containerAdd
  , builderAddFromString, widgetDestroy, labelSetText
  , labelSetEllipsize, labelSetLines
  , builderNew, Button(..), Label(..), Box(..))

data DisplayingNotificationInCenter = DisplayingNotificationInCenter
  { _dpopupContent :: DisplayingNotificationContent
  , _dNotiId :: Int
  , _dNotiDestroy :: IO ()
  , _dLabelTime :: Gtk.Label
  , _dButtonClose :: Gtk.Button
  }
makeClassy ''DisplayingNotificationInCenter
instance HasDisplayingNotificationContent DisplayingNotificationInCenter where
  displayingNotificationContent = dpopupContent


instance Eq DisplayingNotificationInCenter where
  a == b = _dNotiId a == _dNotiId b


showNotification :: Config -> Gtk.Box -> DisplayingNotificationInCenter
                 -> TVar NotifyState
                 -> (DisplayingNotificationInCenter -> IO ())
                 -> IO DisplayingNotificationInCenter
showNotification config mainBox dNoti tNState closeNotification = do
  nState <- readTVarIO tNState
  let (Just noti) = find (\n -> notiId n == _dNotiId dNoti)
        $ notiStList nState

  builder <- Gtk.builderNew
  Gtk.builderAddFromString builder (Text.pack glade) (-1)
  objs <- getObjs builder
    [ "label_time"
    , "button_close" ]

  labelTime <- label objs "label_time"
  buttonClose <- button objs "button_close"

  dispNotiWithoutDestroy <- createNotification config builder noti
    $ dNoti { _dButtonClose = buttonClose
            , _dLabelTime = labelTime
            , _dpopupContent = DisplayingNotificationContent {} }

  let dispNoti = set dNotiDestroy
        (Gtk.widgetDestroy (view dContainer dispNoti)) dispNotiWithoutDestroy
      lblBody = (flip view) dispNoti $ dLabelBody

  setUrgencyLevel (notiUrgency noti) [view dContainer dispNoti]
  setUrgencyLevel (notiUrgency noti)
    $ (flip view) dispNoti
    <$> [dLabelTitel, dLabelBody, dLabelAppname, dLabelTime]

  Gtk.containerAdd mainBox (view dContainer dispNoti)
  updateNoti config mainBox dispNoti tNState

  Gtk.onButtonClicked buttonClose $ do
    closeNotification dispNoti

  Gtk.labelSetEllipsize lblBody
    (if configNotiCenterEllipsizeBody config
     then EllipsizeModeEnd
     else EllipsizeModeNone)

  Gtk.labelSetLines lblBody $ fromIntegral $ configNotiCenterMaxLinesInBody config

  Gtk.widgetShowAll (view dContainer dispNoti)
  return dispNoti


updateNoti :: Config -> Gtk.Box
  -> DisplayingNotificationInCenter -> TVar NotifyState -> IO ()
updateNoti config mainBox dNoti tNState = do
  addSource $ do
    nState <- readTVarIO tNState
    let mNoti = find (\n -> notiId n == _dNotiId dNoti)
          $ notiStList nState
    case mNoti of
      (Just noti) -> do
        updateNotiContent config noti dNoti
        Gtk.labelSetText (_dLabelTime dNoti) $ notiTime noti
        when (configNotiCenterNewFirst config)
          (Gtk.boxReorderChild mainBox (view dContainer dNoti) 0)
      Nothing -> return ()
    return False
  return ()
