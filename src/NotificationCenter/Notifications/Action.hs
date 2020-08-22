{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications.Action
  ( createAction
  , Action(..)
  ) where

import Config (Config(..))
import TransparentWindow

import GI.Gtk
       (widgetSetHalign, widgetSetHexpand, buttonNew, setWidgetMargin
       , buttonSetRelief, widgetSetSizeRequest, widgetShowAll, widgetShow
       , widgetHide, onWidgetDestroy
       , windowSetDefaultSize, setWindowTitle, boxPackStart, boxNew
       , setWindowWindowPosition, WindowPosition(..), windowMove
       , frameSetShadowType, aspectFrameNew
       , widgetGetAllocatedHeight, widgetGetAllocatedWidth, onWidgetDraw
       , onWidgetLeaveNotifyEvent, onWidgetMotionNotifyEvent
       , widgetAddEvents, alignmentSetPadding, alignmentNew, rangeSetValue
       , scaleSetDigits, scaleSetValuePos, rangeGetValue
       , afterScaleButtonValueChanged, scaleNewWithRange, containerAdd
       , buttonBoxNew, mainQuit, onButtonActivate
       , toggleButtonGetActive, onToggleButtonToggled, buttonSetUseStock
       , toggleButtonNewWithLabel, onButtonClicked
       , buttonNewWithLabel, widgetQueueDraw, drawingAreaNew
       , windowNew, widgetDestroy, dialogRun, setAboutDialogComments
       , setAboutDialogAuthors, setAboutDialogVersion
       , setAboutDialogProgramName, aboutDialogNew, labelNew, get
       , afterWindowSetFocus, labelSetText
       , onWidgetFocusOutEvent, onWidgetKeyReleaseEvent, widgetGetParentWindow
       , onButtonClicked, windowGetScreen, boxNew, widgetSetValign
       , imageNewFromIconName)
import GI.Gtk.Enums
       (Orientation(..), PositionType(..), ReliefStyle(..), Align(..), IconSize(..))

import qualified GI.Gtk as Gtk (containerAdd, Box(..), Label(..), Button(..))
import qualified Data.Text as Text
import System.Process (runCommand)

data Action = Action
  { actionButton :: Gtk.Button
    -- ^ Button Element for displaying
  , actionCommand :: String
    -- ^ Shell command to execute
  }

createAction :: Config -> Bool -> (String -> IO ()) -> Int -> Int -> String -> String -> IO Action
createAction config useIcons onAction width height command description = do
  button <- buttonNew
  widgetSetSizeRequest button (fromIntegral width) (fromIntegral height)
  addClass button "userbutton"
  addClass button "deadd-noti-center"
  buttonSetRelief button ReliefStyleNone
  setWidgetMargin button $ fromIntegral $ configButtonMargin config
--  widgetSetHalign label AlignStart
--  widgetSetValign label AlignEnd

  let theButton = Action
        { actionButton = button
        , actionCommand = command }
  onButtonClicked button $ do
    onAction command
    return ()
  if useIcons then do
    img <-imageNewFromIconName (Just $ Text.pack description) (fromIntegral $ fromEnum IconSizeButton)
    Gtk.containerAdd button img
  else do
    label <- labelNew $ Just $ Text.pack description
    addClass label "userbuttonlabel"
    addClass label "deadd-noti-center"
    Gtk.containerAdd button label
  return theButton
