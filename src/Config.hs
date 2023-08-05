{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
  (
    Config(..)
  , ButtonConfig(..)
  , ModificationRule(..)
  , getConfig
  )where

import Data.List.Split (splitOn)
import Helpers (orElse, split, removeOuterLetters, readConfig, replace)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Int ( Int32, Int )
import qualified Data.Map as Map
import qualified Data.Yaml as Y
import Data.Yaml ((.=), (.!=), (.:?), FromJSON(..), (.:))
import Data.Aeson.Key as AesonKey

import System.Process (readCreateProcess, shell)

import NotificationCenter.Notifications.Data
  (notiSendClosedMsg, notiTransient, notiIcon, notiTime, notiAppName
  , notiBody, notiSummary, Notification(..), parseImageString)


data ModificationRule = Modify
  {
    mMatch :: Map.Map String String
  , modifyTitle :: Maybe String
  , modifyBody :: Maybe String
  , modifyAppname :: Maybe String
  , modifyAppicon :: Maybe String
  , modifyTimeout :: Maybe Int32
  , modifyRight :: Maybe Int
  , modifyTop :: Maybe Int
  , modifyImage :: Maybe String
  , modifyImageSize :: Maybe Int
  , modifyTransient :: Maybe Bool
  , modifyNoClosedMsg :: Maybe Bool
  , modifyRemoveActions :: Maybe Bool
  , modifyClassName :: Maybe String
  } |
  Script
  {
    mMatch :: Map.Map String String
  , mScript :: String
  }

instance FromJSON ModificationRule where
  parseJSON (Y.Object o) = do
    mScript <- o .:? "script"
    case mScript of
      Nothing -> Modify
        <$> o .: "match"
      -- modifyTitle
        <*> o .: "modify" .:. "title"
      -- modifyBody
        <*> o .: "modify" .:. "body"
      -- modifyAppname
        <*> o .: "modify" .:. "app-name"
      -- modifyAppicon
        <*> o .: "modify" .:. "app-icon"
      -- modifyTimeout
        <*> o .: "modify" .:. "timeout"
      -- modifyRight
        <*> o .: "modify" .:. "margin-right"
      -- modifyTop
        <*> o .: "modify" .:. "margin-top"
      -- modifyImage
        <*> o .: "modify" .:. "image"
      -- modifyImageSize
        <*> o .: "modify" .:. "image-size"
      -- modifyTransient
        <*> o .: "modify" .:. "transient"
      -- modifyNoClosedMsg
        <*> o .: "modify" .:. "send-noti-closed"
      -- modifyRemoveActions
        <*> o .: "modify" .:. "remove-actions"
      -- modifyClassName
        <*> o .: "modify" .:. "class-name"
      Just (script)-> Script
        <$> o .: "match"
      -- mScript
        <*> return script


data Config = Config
  {
  -- notification-center
    configBarHeight :: Int
  , configBottomBarHeight :: Int
  , configRightMargin :: Int
  , configWidth :: Int
  , configStartupCommand :: String
  , configNotiCenterMonitor :: Int
  , configNotiCenterFollowMouse :: Bool
  , configNotiCenterNewFirst :: Bool
  , configIgnoreTransient :: Bool
  , configMatchingRules :: [ModificationRule]
  , configActionIcons :: Bool
  , configNotiMarkup :: Bool
  , configNotiParseHtmlEntities :: Bool
  , configSendNotiClosedDbusMessage :: Bool
  , configGuessIconFromAppname :: Bool
  , configNotiCenterHideOnMouseLeave :: Bool

  -- notification-center-notification-popup
  , configNotiDefaultTimeout :: Int
  , configDistanceTop :: Int
  , configDistanceRight :: Int
  , configDistanceBetween :: Int
  , configWidthNoti :: Int
  , configNotiFollowMouse :: Bool
  , configNotiMonitor :: Int
  , configImgSize :: Int
  , configImgMarginTop :: Int
  , configImgMarginLeft :: Int
  , configImgMarginBottom :: Int
  , configImgMarginRight :: Int
  , configIconSize :: Int
  , configPopupMaxLinesInBody :: Int
  , configPopupEllipsizeBody :: Bool
  , configPopupDismissButton :: String
  , configPopupDefaultActionButton :: String
  , configPopupHideBodyIfEmpty :: Bool

  -- buttons
  , configButtonsPerRow :: Int
  , configButtonHeight :: Int
  , configButtonMargin :: Int
  , configButtons :: [ButtonConfig]
  }

(.:.) :: FromJSON a => Y.Parser (Maybe Y.Object) -> Text.Text -> Y.Parser (Maybe  a)
(.:.) po name = do
  mO <- po
  case mO of
    Nothing -> return Nothing
    (Just x) -> x .:? AesonKey.fromString (Text.unpack name)

(.!=>) :: FromJSON a => Y.Parser (Maybe a) -> Y.Parser (Maybe a) -> Y.Parser (Maybe a)
(.!=>) a b = orElse <$> a <*> b

firstLevel o firstKey alt =
  o .:? firstKey
  .!= alt

secondLevel o firstKey secondKey alt =
  o .:? firstKey .:. secondKey
  .!= alt

thirdLevel o firstKey secondKey thirdKey alt =
  o .:? firstKey .:. secondKey .:. thirdKey
  .!= alt

fourthLevel o firstKey secondKey thirdKey fourthKey alt =
  o .:? firstKey .:. secondKey .:. thirdKey .:. fourthKey
  .!= alt

inheritingSecondLevel o firstKey secondKey alt =
  o .:? firstKey .:. secondKey
  .!=> (o .:? AesonKey.fromString (Text.unpack secondKey))
  .!= alt

inheritingThirdLevel o firstKey secondKey thirdKey alt =
  o .:? firstKey .:. secondKey .:. thirdKey
  .!=> (o .:? AesonKey.fromString (Text.unpack secondKey) .:. thirdKey)
  .!=> (o .:? AesonKey.fromString (Text.unpack thirdKey))
  .!= alt

instance FromJSON Config where
  parseJSON (Y.Object o) =
    Config
  --configBarHeight
    <$> inheritingSecondLevel o "notification-center" "margin-top" 0
  -- configBottomBarHeight
    <*> inheritingSecondLevel o "notification-center" "margin-bottom" 0
  -- configRightMargin
    <*> inheritingSecondLevel o "notification-center" "margin-right" 0
  -- configWidth
    <*> inheritingSecondLevel o "notification-center" "width" 500
  -- configStartupCommand
    <*> firstLevel o "startup-command" ""
  -- configNotiCenterMonitor
    <*> inheritingSecondLevel o "notification-center" "monitor" 0
  -- configNotiCenterFollowMouse
    <*> inheritingSecondLevel o "notification-center" "follow-mouse" False
  -- configNotiCenterNewFirst
    <*> secondLevel o "notification-center" "new-first" True
  -- configIgnoreTransient
    <*> secondLevel o "notification-center" "ignore-transient" False
  -- configMatchingRules
    <*> secondLevel o "notification" "modifications" []
  -- configActionIcons
    <*> secondLevel o "notification" "use-action-icons" True
  -- configNotiMarkup
    <*> secondLevel o "notification" "use-markup" True
  -- configNotiParseHtmlEntities
    <*> secondLevel o "notification" "parse-html-entities" True
  -- configSendNotiClosedDbusMessage
    <*> thirdLevel o "notification" "dbus" "send-noti-closed" False
  -- configGuessIconFromAppname
    <*> inheritingThirdLevel o "notification" "app-icon" "guess-icon-from-name"
    True
  -- configNotiCenterHideOnMouseLeave
    <*> secondLevel o "notification-center" "hide-on-mouse-leave" True
  -- configNotiDefaultTimeout
    <*> thirdLevel o "notification" "popup" "default-timeout" 1000
  -- configDistanceTop
    <*> inheritingThirdLevel o "notification" "popup" "margin-top" 50
  -- configDistanceRight
    <*> inheritingThirdLevel o "notification" "popup" "margin-right" 50
  -- configDistanceBetween
    <*> thirdLevel o "notification" "popup" "margin-between" 20
  -- configWidthNoti
    <*> inheritingThirdLevel o "notification" "popup" "width" 300
  -- configNotiFollowMouse
    <*> inheritingThirdLevel o "notification" "popup" "follow-mouse" False
  -- configNotiMonitor
    <*> inheritingThirdLevel o "notification" "popup" "monitor" 0
  -- configImgSize
    <*> thirdLevel o "notification" "image" "size" 100
  -- configImgMarginTop
    <*> thirdLevel o "notification" "image" "margin-top" 15
  -- configImgMarginLeft
    <*> thirdLevel o "notification" "image" "margin-left" 15
  -- configImgMarginBottom
    <*> thirdLevel o "notification" "image" "margin-bottom" 15
  -- configImgMarginRight
    <*> thirdLevel o "notification" "image" "margin-right" 0
  -- configIconSize
    <*> thirdLevel o "notification" "app-icon" "icon-size" 20
  -- configPopupMaxLinesInBody
    <*> inheritingThirdLevel o "notification" "popup" "max-lines-in-body" 3
  -- configPopupEllipsizeBody
    <*> ((/= (0 :: Int)) <$>
          inheritingThirdLevel o "notification" "popup" "max-lines-in-body" 3)
  -- configPopupDismissButton
    <*> fourthLevel o "notification" "popup" "click-behavior" "dismiss" "mouse1"
  -- configPopupDefaultActionButton
    <*> fourthLevel o "notification" "popup" "click-behavior" "default-action" "mouse3"
  -- configPopupHideBodyIfEmpty
    <*> thirdLevel o "notification" "popup" "hide-body-if-empty" False
  -- configButtonsPerRow
    <*> thirdLevel o "notification-center" "buttons" "buttons-per-row" 5
  -- configButtonHeight
    <*> thirdLevel o "notification-center" "buttons" "buttons-height" 60
  -- configButtonMargin
    <*> thirdLevel o "notification-center" "buttons" "buttons-margin" 2
  -- configLabels
    <*> thirdLevel o "notification-center" "buttons" "actions" []
  parseJSON _ = fail "Expected Object for Config value"

data ButtonConfig = Button
  {
    configButtonLabel :: String
  , configButtonCommand :: String
  }

instance FromJSON ButtonConfig where
  parseJSON (Y.Object o) = Button
        <$> o .: "label"
        <*> o .: "command"


getConfig :: Text.Text -> IO Config
getConfig configYml = Y.decodeThrow $ encodeUtf8 configYml
