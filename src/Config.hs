{-# LANGUAGE OverloadedStrings #-}

module Config
  (
    Config(..)
  , getConfig
  )where

import Data.List.Split (splitOn)
import Helpers (orElse, split, removeOuterLetters, readConfig, replace)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Int ( Int32, Int )
import qualified Data.Map as Map
import qualified Data.Yaml as Y
import Data.Yaml ((.=), (.!=), (.:?), FromJSON(..), (.:))

import System.Process (readCreateProcess, shell)

import NotificationCenter.Notifications.Data
  (notiSendClosedMsg, notiTransient, notiIcon, notiTime, notiAppName
  , notiBody, notiSummary, Notification(..), parseImageString)


data MatchingRules = Modify
  {
    mMatch :: [(String, String)]
  , modifyTitle :: Maybe String
  , modifyBody :: Maybe String
  , modifyAppname :: Maybe String
  , modifyAppicon :: Maybe String
  , modifyTimeout :: Maybe Int
  , modifyRight :: Maybe Int
  , modifyTop :: Maybe Int
  , modifyImage :: Maybe String
  , modifyTransient :: Maybe Bool
  , modifyNoClosedMsg :: Maybe Bool
  , modifyRemoveActions :: Maybe Bool
  } |
  Script
  {
    mMatch :: [(String, String)]
  , mScript :: String
  }
instance FromJSON MatchingRules where
  parseJSON (Y.Object o) = do
    mScript <- o .:? "script"
    case mScript of
      Nothing -> Modify
        <$> (parseJSON =<< (o .: "match"))
      -- modifyTitle
        <*> o .:? "title"
      -- modifyBody
        <*> o .:? "body"
      -- modifyAppname
        <*> o .:? "appname"
      -- modifyAppicon
        <*> o .:? "appicon"
      -- modifyTimeout
        <*> o .:? "timeout"
      -- modifyRight
        <*> o .:? "margin-right"
      -- modifyTop
        <*> o .:? "margin-top"
      -- modifyImage
        <*> o .:? "image"
      -- modifyTransient
        <*> o .:? "transient"
      -- modifyNoClosedMsg
        <*> o .:? "noClosedMsg"
      -- modifyRemoveActions
        <*> o .:? "removeActions"
      Just (script)-> Script
        <$> (parseJSON =<< (o .: "match"))
      -- modifyTitle
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
  , configMatchingRules :: [((Notification -> Bool), (Notification -> IO Notification))]
  , configNotiMarkup :: Bool
  , configNotiParseHtmlEntities :: Bool
  , configSendNotiClosedDbusMessage :: Bool
  , configGuessIconFromAppname :: Bool
  , configNotiCenterMaxLinesInBody :: Int
  , configNotiCenterEllipsizeBody :: Bool
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

  -- buttons
  , configButtonsPerRow :: Int
  , configButtonHeight :: Int
  , configButtonMargin :: Int
  , configLabels :: String
  , configCommands :: String
  }

altVal :: FromJSON a => Y.Object -> Text.Text ->  a -> Y.Parser a
altVal v name altval = fromMaybe altval <$> (v .:? name)

altField :: FromJSON a => Y.Object -> Text.Text -> Y.Parser a -> Y.Parser a
altField v name altval = fromMaybe <$> altval <*> (v .:? name)

obj :: Y.Object -> Text.Text -> Y.Parser Y.Object
obj v name = v .: name

-- objV :: FromJSON a => Y.Parser ( Y.Object) -> Text.Text -> Y.Parser (Maybe  a)
-- objV po name = (flip (.:?)) name =<< po

(.:.) :: FromJSON a => Y.Parser (Maybe Y.Object) -> Text.Text -> Y.Parser (Maybe  a)
(.:.) po name = do
  mO <- po
  case mO of
    Nothing -> return Nothing
    (Just x) -> x .:? name

(.!=>) :: FromJSON a => Y.Parser (Maybe a) -> Y.Parser (Maybe a) -> Y.Parser (Maybe a)
(.!=>) a b = orElse <$> a <*> b

oneDeep o lone ltwo alt = o .:? lone .:. ltwo .!= alt
overwriteOneDeep o lone ltwo alt = o .:? lone .:. ltwo .!=> (o .: ltwo) .!= alt

twoDeep o lone ltwo lthree alt = o .:? lone .:. ltwo .:. lthree .!= alt
overwriteLastTwoDeep o lone ltwo lthree alt = o .:? lone .:. ltwo .:. lthree
  .!=> (o .:? ltwo .:. lthree) .!= alt

overwriteWithFirstTwoDeep o lone ltwo lthree alt =
  o .:? lone .:. ltwo .:. lthree
  .!=> (o .:? lone) .!= alt

threeDeep o lone ltwo lthree lfour alt = o .:? lone .:. ltwo .:. lthree .:. lfour .!= alt

instance FromJSON Config where
  parseJSON (Y.Object o) =
    Config
  --configBarHeight
    <$> overwriteOneDeep o "notification-center" "margin-top" 0
  -- configBottomBarHeight
    <*> overwriteOneDeep o "notification-center" "margin-bottom" 0
  -- configRightMargin
    <*> overwriteOneDeep o "notification-center" "margin-right" 0
  -- configWidth
    <*> overwriteOneDeep o "notification-center" "width" 500
  -- configStartupCommand
    <*> o .:? "startup-command" .!= ""
  -- configNotiCenterMonitor
    <*> oneDeep o "notification-center" "monitor" 500
  -- configNotiCenterFollowMouse
    <*> overwriteOneDeep o "notification-center" "follow-mouse" False
  -- configNotiCenterNewFirst
    <*> oneDeep o "notification-center" "new-first" True
  -- configIgnoreTransient
    <*> oneDeep o "notification-center" "ignore-transient" False


  -- configMatchingRules
    <*> getModifications o
  -- configNotiMarkup
    <*> oneDeep o "notification" "use-markup" True
  -- configNotiParseHtmlEntities
    <*> oneDeep o "notification" "parse-html-entities" True
  -- configSendNotiClosedDbusMessage
    <*> twoDeep o "notification" "dbus" "send-noti-closed" False
  -- configGuessIconFromAppname
    <*> overwriteLastTwoDeep o "notification" "app-icon" "guess-icon-from-name"
    True
  -- configNotiCenterMaxLinesInBody
    <*> overwriteLastTwoDeep o "notification" "in-center" "max-lines-in-body" 0
  -- configNotiCenterEllipsizeBody
    <*> ((/= (0 :: Int)) <$>
          overwriteLastTwoDeep o "notification" "in-center" "max-lines-in-body" 0)
  -- configNotiCenterHideOnMouseLeave
    <*> oneDeep o "notification-center" "hide-on-mouse-leave" True
  -- configNotiDefaultTimeout
    <*> twoDeep o "notification" "popup" "default-timeout" 1000
  -- configDistanceTop
    <*> overwriteWithFirstTwoDeep o "notification" "popup" "margin-top" 50
  -- configDistanceRight
    <*> overwriteWithFirstTwoDeep o "notification" "popup" "margin-top" 50
  -- configDistanceBetween
    <*> twoDeep o "notification" "popup" "margin-between" 20
  -- configWidthNoti
    <*> overwriteWithFirstTwoDeep o "notification" "popup" "width" 300
  -- configNotiFollowMouse
    <*> overwriteWithFirstTwoDeep o "notification" "popup" "follow-mouse" False
  -- configNotiMonitor
    <*> overwriteWithFirstTwoDeep o "notification" "popup" "monitor" 0
  -- configImgSize
    <*> twoDeep o "notification" "image" "size" 100
  -- configImgMarginTop
    <*> twoDeep o "notification" "image" "margin-top" 15
  -- configImgMarginLeft
    <*> twoDeep o "notification" "image" "margin-left" 15
  -- configImgMarginBottom
    <*> twoDeep o "notification" "image" "margin-bottom" 15
  -- configImgMarginRight
    <*> twoDeep o "notification" "image" "margin-right" 0
  -- configIconSize
    <*> twoDeep o "notification" "app-icon" "icon-size" 20
  -- configPopupMaxLinesInBody
    <*> overwriteLastTwoDeep o "notification" "pop-up" "max-lines-in-body" 3
  -- configPopupEllipsizeBody
    <*> ((/= (0 :: Int)) <$>
          overwriteLastTwoDeep o "notification" "pop-up" "max-lines-in-body" 3)
  -- configPopupDismissButton
    <*> threeDeep o "notification" "popup" "click-behavior" "dismiss ""mouse1"
  -- configPopupDefaultActionButton
    <*> threeDeep o "notification" "popup" "click-behavior" "default-action ""mouse3"
  -- configButtonsPerRow
    <*> twoDeep o "notification-center" "buttons" "buttons-per-row" 5
  -- configButtonHeight
    <*> twoDeep o "notification-center" "buttons" "buttons-height" 60
  -- configButtonMargin
    <*> twoDeep o "notification-center" "buttons" "buttons-height" 2
  -- configLabels
    <*> (fst <$> getButtons o)
  -- configCommands
    <*> (snd <$> getButtons o)
  parseJSON _ = fail "Expected Object for Config value"

getButtons :: Y.Object -> Y.Parser (String, String)
getButtons o = return ("", "") -- twoDeep o "notification-center" "buttons" "buttons-heightasrti" ("", "")

getModifications :: Y.Object -> Y.Parser [((Notification -> Bool), (Notification -> IO Notification))]
getModifications o = do
  matchingRules <- (parseJSON =<< (fromMaybe (return []) <$> oneDeep o "notification" "modifications")) :: [MatchingRules]
  return [] -- oneDeep o "notification" "modifications" [] --todo



getConfig configYml =
  Y.decodeThrow $ encodeUtf8 configYml
--  Config
--  {
--    -- notification-center
--    configBarHeight = r 0 p nCenter "marginTop"
--  , configBottomBarHeight = r 0 p nCenter "marginBottom"
--  , configRightMargin = r 0 p nCenter "marginRight"
--  , configWidth = r 500 p nCenter "width"
--  , configStartupCommand = r' "" p nCenter "startupCommand"
--  , configNotiCenterMonitor = r 0 p nCenter "monitor"
--  , configNotiCenterFollowMouse = r'' False p nCenter "followMouse"
--  , configNotiCenterNewFirst = r'' True p nCenter "newFirst"
--  , configIgnoreTransient = r'' False p nCenter "ignoreTransient"
--  , configNotiMarkup = r'' True p nCenter "useMarkup"
--  , configNotiParseHtmlEntities = r'' True p nCenter "parseHtmlEntities"
--  , configSendNotiClosedDbusMessage = r'' False p nCenter "configSendNotiClosedDbusMessage"
--  , configGuessIconFromAppname = r'' True p nCenter "guessIconFromAppname"
--  , configNotiCenterMaxLinesInBody = r (-1) p nCenter "shortenBody"
--  , configNotiCenterEllipsizeBody = (r (-1) p nCenter "shortenBody") /= -1
--  , configNotiCenterHideOnMouseLeave = r'' True p nCenter "hideOnMouseLeave"
--  , configMatchingRules =
--      zip (map (matchingRule) $ splitRulesets $ r' "" p nCenter "match")
--      ((map (modification) $ splitRulesets $ r' "" p nCenter "modify")
--        ++ repeat (return . id))
--
--    -- notification-center-notification-popup
--  , configNotiDefaultTimeout = r 10000 p nPopup "notiDefaultTimeout"
--  , configDistanceTop = r 50 p nPopup "distanceTop"
--  , configDistanceRight = r 50 p nPopup "distanceRight"
--  , configDistanceBetween = r 20 p nPopup "distanceBetween"
--  , configWidthNoti = r 300 p nPopup "width"
--  , configNotiMonitor = r 0 p nPopup "monitor"
--  , configNotiFollowMouse = r'' False p nPopup "followMouse"
--  , configImgSize = r 100 p nPopup "maxImageSize"
--  , configImgMarginTop = r 15 p nPopup "imageMarginTop"
--  , configImgMarginBottom = r 15 p nPopup "imageMarginBottom"
--  , configImgMarginLeft = r 15 p nPopup "imageMarginLeft"
--  , configImgMarginRight = r 0 p nPopup "imageMarginRight"
--  , configIconSize = r 20 p nPopup "iconSize"
--  , configPopupMaxLinesInBody = r 5 p nPopup "shortenBody"
--  , configPopupEllipsizeBody = (r 5 p nPopup "shortenBody") /= -1
--  , configPopupDismissButton = r' "mouse1" p nPopup "dismissButton"
--  , configPopupDefaultActionButton = r' "mouse3" p nPopup "defaultActionButton"
--
--    -- buttons
--  , configButtonsPerRow = r 5 p buttons "buttonsPerRow"
--  , configButtonHeight = r 60 p buttons "buttonHeight"
--  , configButtonMargin = r 2 p buttons "buttonMargin"
--  , configLabels = r' "" p buttons "labels"
--  , configCommands = r' "" p buttons "commands"
--
--  }
--  where
--    nPopup = "notification-center-notification-popup"
--    nCenter = "notification-center"
--    buttons = "buttons"
--    r = readConfig
--    r' = readConfig
--    r'' = readConfig
--    splitRulesets :: String -> [[String]]
--    splitRulesets text = splitOn ";" <$> (split $ removeOuterLetters $ text)


keys :: [String] -> [[String]]
keys conditions = map (\c -> if length (splitted c) == 2
                             then (splitted c) else ["", ""]) conditions
  where splitted condition = splitOn "=" condition


matchingRule :: [String] -> Notification -> Bool
matchingRule  conditions noti = foldl (
  \matches (k:v:[]) -> matches && ((v == lookupFun k noti)))
                                True (keys conditions)
  where lookupFun name noti = Text.unpack $ fromMaybe (Text.pack "")
          ((lookup name
             [ ("title", notiSummary)
             , ("body", notiBody)
             , ("app", notiAppName)
             , ("time", notiTime) ]) <*> (Just noti))


modification :: [String] -> Notification -> IO Notification
modification condition noti = foldl (
  \noti (k:v:[]) -> modifyNotification k v =<< noti)
  (return noti) (keys condition)

modifyNotification :: String -> String -> Notification -> IO Notification
modifyNotification k v noti
  | k == "script" = do
      putStrLn $ show noti
      putStrLn $ Text.unpack $ notiBody noti
      returnText <- readCreateProcess (shell v) $ show noti
      modification (splitOn ";" returnText) noti
  | k == "title" = return $ noti { notiSummary = Text.pack v }
  | k == "body" = return $ noti { notiBody = Text.pack v }
  | k == "app" = return $ noti { notiAppName = Text.pack v }
  | k == "time" =
      return $ noti { notiTime = Text.pack v }
  | k == "timeout" =
      return $ noti { notiTimeout = read v :: Int32 }
  | k == "right" =
      return $ noti { notiRight = Just . (read :: String -> Int) $ v }
  | k == "top" =
      return $ noti { notiTop = Just . (read :: String -> Int) $ v }
  | k == "icon" =
      return $ noti { notiIcon = parseImageString $ Text.pack v }
  | k == "image" =
      return $ noti { notiImg = parseImageString $ Text.pack v }
  | k == "transient" && v == "true" =
      return $ noti { notiTransient = True }
  | k == "transient" && v == "false" =
      return $ noti { notiTransient = False }
  | k == "noClosedMsg" && v == "true" =
      return $ noti { notiSendClosedMsg = False }
  | k == "removeActions" = return $ noti { notiActions = [] }
  | otherwise = return noti
