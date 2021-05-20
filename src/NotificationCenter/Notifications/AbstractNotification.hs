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

import Helpers (atMay, markupify)
import Config (Config(..))
import NotificationCenter.Notifications.Data
  (Urgency(..), CloseType(..), Notification(..), Image(..), rawImgToPixBuf)
import NotificationCenter.Notifications.Action
  (Action(..), createAction)
import TransparentWindow (scale, label, image, box, getObjs, addClass, progressbar)

import Data.Text as Text
import Data.Int ( Int32 )
import Data.Maybe (fromMaybe)

import Control.Lens.TH (makeClassy)
import Control.Lens (view, set)
import Control.Monad (when)

import GI.Gtk (rangeGetValue, onRangeValueChanged, rangeSetValue, widgetShowAll, widgetHide, windowMove, widgetDestroy
              , widgetSetValign, widgetSetMarginStart, widgetSetMarginEnd 
              , widgetSetMarginTop, widgetSetMarginBottom, labelSetText 
              , labelSetMarkup, widgetSetSizeRequest, labelSetXalign 
              , widgetGetPreferredHeightForWidth, onWidgetButtonPressEvent
              , imageSetFromPixbuf, imageSetFromIconName, setWidgetWidthRequest
              , setImagePixelSize, widgetSetMarginStart, widgetSetMarginEnd
              , progressBarSetFraction, widgetSetVisible
              , catchGErrorJustDomain, GErrorMessage(..))
import GI.GLib (FileError(..))
import GI.GdkPixbuf (pixbufScaleSimple, pixbufGetHeight, pixbufGetWidth
                    , Pixbuf(..), pixbufNewFromFileAtScale
                    , InterpType(..), PixbufError(..))
import qualified GI.Gtk as Gtk
  (Scale, ProgressBar, IsWidget, Box(..), Label(..), Button(..), Window(..), Image(..)
  , Builder(..), containerAdd, containerRemove, containerGetChildren)
import GI.Gtk.Enums (Align(..))

data DisplayingNotificationContent = DisplayingNotificationContent
  { _dLabelTitel :: Gtk.Label
  , _dLabelBody :: Gtk.Label
  , _dLabelAppname :: Gtk.Label
  , _dImgAppIcon :: Gtk.Image
  , _dImgImage :: Gtk.Image
  , _dContainer :: Gtk.Box
  , _dActions :: Gtk.Box
  , _dProgressbar :: Gtk.ProgressBar
  , _dScale :: Gtk.Scale
  }
makeClassy ''DisplayingNotificationContent



createNotification :: HasDisplayingNotificationContent dn =>
  Config -> Gtk.Builder -> Notification -> dn -> IO dn
createNotification config builder noti dispNoti = do

  objs <- getObjs builder [ "label_titel"
                          , "label_body"
                          , "label_appname"
                          , "img_icon"
                          , "img_img"
                          , "box_container"
                          , "box_actions"
                          , "progressbar"
                          , "scale"]

  labelTitel <- label objs "label_titel"
  labelBody <- label objs "label_body"
  labelAppname <- label objs "label_appname"
  container <- box objs "box_container"
  actions <- box objs "box_actions"
  imgAppIcon <- image objs "img_icon"
  imgImage <- image objs "img_img"
  progressBar <- progressbar objs "progressbar"
  scale <- scale objs "scale"

  -- set margins from config
  widgetSetMarginTop imgImage
    (fromIntegral $ configImgMarginTop config)
  widgetSetMarginBottom imgImage
    (fromIntegral $ configImgMarginBottom config)
  widgetSetMarginStart imgImage
    (fromIntegral $ configImgMarginLeft config)
  widgetSetMarginEnd imgImage
    (fromIntegral $ configImgMarginRight config)

  onWidgetButtonPressEvent container $ \(_) -> do
    notiOnAction noti "default" Nothing
    return False


  return
    $ set dLabelTitel labelTitel
    $ set dLabelBody labelBody
    $ set dLabelAppname labelAppname
    $ set dImgAppIcon imgAppIcon
    $ set dImgImage imgImage
    $ set dContainer container
    $ set dActions actions
    $ set dProgressbar progressBar
    $ set dScale scale
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
  let iconSize = 15
      imageSize = 100
  setImage (notiIcon noti) (fromIntegral $ configIconSize config)
    $ view dImgAppIcon dNoti
  setImage (notiImg noti) (fromIntegral $ configImgSize config)
    $ view dImgImage dNoti

  let takeTwo (a:b:cs) = (a,b):(takeTwo cs)
      takeTwo _ = []
  actionButtons <- sequence
    $ (\(a, b) -> createAction config (notiActionIcons noti) (notiOnAction noti) 20 20 a b)
    <$> (Prelude.filter (\(a, b) -> a /= "default"
                           && (notiPercentage noti == Nothing
                               || a /= "changeValue"))
         $ takeTwo (unpack <$> notiActions noti))
  currentButtons <- Gtk.containerGetChildren (view dActions dNoti)
  sequence $ Gtk.containerRemove (view dActions dNoti) <$> currentButtons
  sequence $ Gtk.containerAdd (view dActions dNoti) <$> actionButton <$> actionButtons

  widgetShowAll (view dActions dNoti)


  if (notiPercentage noti /= Nothing) then do
    if (onChangeAction == Nothing) then do
      progressBarSetFraction (view dProgressbar dNoti)
        ((fromMaybe 0 $ notiPercentage noti) / 100.0)
      widgetSetVisible (view dProgressbar dNoti) True
      widgetSetVisible (view dScale dNoti) False
      return ()
      else do
      rangeSetValue (view dScale dNoti)
        (fromMaybe 0 $ notiPercentage noti)
      onRangeValueChanged (view dScale dNoti) $ do
        value <- rangeGetValue (view dScale dNoti)
        (notiOnAction noti) "changeValue" $ Just $ show value
        return ()
      widgetSetVisible (view dScale dNoti) True
      widgetSetVisible (view dProgressbar dNoti) False
      return ()
    else do
    widgetSetVisible (view dProgressbar dNoti) False
    widgetSetVisible (view dScale dNoti) False
  return ()
  where  onChangeAction = atMay
           (Prelude.filter (\(a, b) -> a == "changeValue")
            $ takeTwo (unpack <$> notiActions noti)) 0
         takeTwo (a:b:cs) = (a,b):(takeTwo cs)
         takeTwo _ = []



setImage :: Image -> Int32 -> Gtk.Image -> IO ()
setImage image imageSize widget = do
  case image of
    NoImage -> do
      widgetSetMarginStart widget 0
      widgetSetMarginEnd widget 0
    (ImagePath path) -> do
      pb <- catchGErrorJustDomain
            (catchGErrorJustDomain
             (Just <$> pixbufNewFromFileAtScale path imageSize imageSize True)
             ((\err message -> return Nothing)
              :: PixbufError -> GErrorMessage -> IO (Maybe (Maybe Pixbuf))))
            ((\err message -> return Nothing)
              :: FileError -> GErrorMessage -> IO (Maybe (Maybe Pixbuf)))
      case pb of
        (Just pb') -> case pb' of
                        (Just pb'') -> imageSetFromPixbuf widget (Just pb'')
                        Nothing -> return ()
        Nothing -> return ()
    (NamedIcon name) -> do
      imageSetFromIconName widget
        (Just $ pack name) imageSize
      setImagePixelSize widget imageSize
    (RawImg a) -> do
      pb <- rawImgToPixBuf $ RawImg a
      pb' <- scalePixbuf imageSize imageSize pb
      imageSetFromPixbuf widget pb'


scalePixbuf :: Int32 -> Int32 -> Pixbuf -> IO (Maybe Pixbuf)
scalePixbuf w h pb = do
  oldW <- fromIntegral <$> pixbufGetWidth pb :: IO Double
  oldH <- fromIntegral <$> pixbufGetHeight pb :: IO Double
  let targetW = fromIntegral w :: Double
      targetH = fromIntegral h :: Double
  if (oldW < targetW || oldH < targetH) then
    return (Just pb)
    else do
    let newW = fromIntegral $ floor $ if oldW > oldH then
          targetW else (oldW * (targetH / oldH))
        newH = fromIntegral $ floor $ if oldW > oldH then
          (oldH * (targetW / oldW)) else targetH
    pixbufScaleSimple pb newW newH InterpTypeBilinear
