{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Button
  ( createButton
  , setButtonState
  , Button(..)
  ) where

import Config (Config(..))
import TransparentWindow

import qualified GI.Gtk as Gtk

import GI.Gtk.Enums
       (Orientation(..), PositionType(..), Align(..))

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
  button <- Gtk.buttonNew
  label <- Gtk.labelNew $ Just $ Text.pack description
  Gtk.widgetSetSizeRequest button (fromIntegral width) (fromIntegral height)
  addClass button "userbutton"
  addClass button "deadd-noti-center"
  Gtk.buttonSetHasFrame button False
  Gtk.setWidgetMarginTop button $ fromIntegral $ configButtonMargin config
  Gtk.setWidgetMarginBottom button $ fromIntegral $ configButtonMargin config
  Gtk.setWidgetMarginStart button $ fromIntegral $ configButtonMargin config
  Gtk.setWidgetMarginEnd button $ fromIntegral $ configButtonMargin config
  Gtk.widgetSetHalign label AlignStart
  Gtk.widgetSetValign label AlignEnd
  Gtk.widgetAddCssClass label "userbuttonlabel"
  Gtk.widgetAddCssClass label "deadd-noti-center"

  let theButton = Button
        { buttonButton = button
        , buttonLabel = label
        , buttonCommand = command }
  Gtk.onButtonClicked button $ do
    addSource $ do
      setButtonState2 $ theButton
      return False
    runCommand command
    return ()
  Gtk.buttonSetChild button $ Just label
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
