{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Button
  ( createButton
  , setButtonState
  , Button(..)
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
       , onButtonClicked, windowGetScreen, boxNew, widgetSetValign)
import GI.Gtk.Enums
       (Orientation(..), PositionType(..), ReliefStyle(..), Align(..))

import qualified GI.Gtk as Gtk (containerAdd, Box(..), Label(..), Button(..))
import qualified Data.Text as Text
import System.Process (runCommand)

data Button = Button
  { buttonButton :: Gtk.Button
    -- ^ Button Element for displaying
  , buttonLabel :: Gtk.Label
    -- ^ Description text label
  , buttonCommand :: String
    -- ^ Shell command to execute
  }

createButton :: Config -> Int -> Int -> String -> String -> IO Button
createButton config width height command description = do
  button <- buttonNew
  label <- labelNew $ Just $ Text.pack description
  widgetSetSizeRequest button (fromIntegral width) (fromIntegral height)
  addClass button "userbutton"
  addClass button "deadd-noti-center"
  buttonSetRelief button ReliefStyleNone
  setWidgetMargin button $ fromIntegral $ configButtonMargin config
  widgetSetHalign label AlignStart
  widgetSetValign label AlignEnd
  addClass label "userbuttonlabel"
  addClass label "deadd-noti-center"

  let theButton = Button
        { buttonButton = button
        , buttonLabel = label
        , buttonCommand = command }
  onButtonClicked button $ do
    addSource $ do
      setButtonState2 $ theButton
      return False
    runCommand command
    return ()
  Gtk.containerAdd button label
  return theButton

setButtonState2 :: Button -> IO ()
setButtonState2 button = do
  addClass (buttonButton button) "buttonState2"
  addClass (buttonLabel button) "buttonState2"

setButtonState :: Button -> Bool -> IO ()
setButtonState button state = do
  removeClass (buttonButton button) "buttonState2"
  removeClass (buttonLabel button) "buttonState2"
  if state then
    do
      addClass (buttonButton button) "buttonState1"
      addClass (buttonLabel button) "buttonState1"
    else
    do
      removeClass (buttonButton button) "buttonState1"
      removeClass (buttonLabel button) "buttonState1"
