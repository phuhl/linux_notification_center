{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications.Action
  ( createAction
  , Action(..)
  ) where

import Config (Config(..))
import TransparentWindow

import qualified GI.Gtk as Gtk
import GI.Gtk.Enums
       (Orientation(..), PositionType(..), Align(..), IconSize(..))

import qualified Data.Text as Text
import System.Process (runCommand)

data Action = Action
  { actionButton :: Gtk.Button
    -- ^ Button Element for displaying
  , actionCommand :: String
    -- ^ Shell command to execute
  }

createAction :: Config -> Bool -> (String -> Maybe String -> IO ()) -> Int -> Int -> String -> String -> IO Action
createAction config useIcons onAction width height command description = do
  button <- Gtk.buttonNew
  Gtk.widgetSetSizeRequest button (fromIntegral width) (fromIntegral height)
  addClass button "userbutton"
  addClass button "deadd-noti-center"
  Gtk.buttonSetHasFrame button False
  Gtk.setWidgetMarginTop button $ fromIntegral $ configButtonMargin config
  Gtk.setWidgetMarginBottom button $ fromIntegral $ configButtonMargin config
  Gtk.setWidgetMarginStart button $ fromIntegral $ configButtonMargin config
  Gtk.setWidgetMarginEnd button $ fromIntegral $ configButtonMargin config
--  widgetSetHalign label AlignStart
--  widgetSetValign label AlignEnd

  let theButton = Action
        { actionButton = button
        , actionCommand = command }
  Gtk.onButtonClicked button $ do
    onAction command Nothing
    return ()
  if useIcons && configActionIcons config then do
    img <- Gtk.imageNewFromIconName (Just $ Text.pack description)
    Gtk.buttonSetChild button $ Just img
  else do
    label <- Gtk.labelNew $ Just $ Text.pack description
    addClass label "userbuttonlabel"
    addClass label "deadd-noti-center"
    Gtk.buttonSetChild button $ Just label
  return theButton
