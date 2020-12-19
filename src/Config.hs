
module Config
  (
    Config(..)
  , getConfig
  )where

import Data.List.Split (splitOn)
import Helpers (split, removeOuterLetters, readConfig, replace)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Int ( Int32, Int )
import qualified Data.Map as Map

import NotificationCenter.Notifications.Data
  (notiSendClosedMsg, notiTransient, notiIcon, notiTime, notiAppName
  , notiBody, notiSummary, Notification(..), parseImageString)


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
  , configMatchingRules :: [((Notification -> Bool), (Notification -> Notification), Maybe String)]
  , configNotiMarkup :: Bool
  , configNotiParseHtmlEntities :: Bool
  , configSendNotiClosedDbusMessage :: Bool
  , configGuessIconFromAppname :: Bool
  , configNotiCenterMaxLinesInBody :: Int
  , configNotiCenterEllipsizeBody :: Bool

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


getConfig p =
  Config
  {
    -- notification-center
    configBarHeight = r 0 p nCenter "marginTop"
  , configBottomBarHeight = r 0 p nCenter "marginBottom"
  , configRightMargin = r 0 p nCenter "marginRight"
  , configWidth = r 500 p nCenter "width"
  , configStartupCommand = r' "" p nCenter "startupCommand"
  , configNotiCenterMonitor = r 0 p nCenter "monitor"
  , configNotiCenterFollowMouse = r'' False p nCenter "followMouse"
  , configNotiCenterNewFirst = r'' True p nCenter "newFirst"
    , configIgnoreTransient = r'' False p nCenter "ignoreTransient"
  , configMatchingRules = zip3 match (modify ++ repeat id) $ run ++ repeat Nothing -- run
  , configNotiMarkup = r'' True p nCenter "useMarkup"
  , configNotiParseHtmlEntities = r'' True p nCenter "parseHtmlEntities"
  , configSendNotiClosedDbusMessage = r'' False p nCenter "configSendNotiClosedDbusMessage"
  , configGuessIconFromAppname = r'' True p nCenter "guessIconFromAppname"
  , configNotiCenterMaxLinesInBody = r (-1) p nCenter "shortenBody"
  , configNotiCenterEllipsizeBody = (r (-1) p nCenter "shortenBody") /= -1

    -- notification-center-notification-popup
  , configNotiDefaultTimeout = r 10000 p nPopup "notiDefaultTimeout"
  , configDistanceTop = r 50 p nPopup "distanceTop"
  , configDistanceRight = r 50 p nPopup "distanceRight"
  , configDistanceBetween = r 20 p nPopup "distanceBetween"
  , configWidthNoti = r 300 p nPopup "width"
  , configNotiMonitor = r 0 p nPopup "monitor"
  , configNotiFollowMouse = r'' False p nPopup "followMouse"
  , configImgSize = r 100 p nPopup "maxImageSize"
  , configImgMarginTop = r 15 p nPopup "imageMarginTop"
  , configImgMarginBottom = r 15 p nPopup "imageMarginBottom"
  , configImgMarginLeft = r 15 p nPopup "imageMarginLeft"
  , configImgMarginRight = r 0 p nPopup "imageMarginRight"
  , configIconSize = r 20 p nPopup "iconSize"
  , configPopupMaxLinesInBody = r 5 p nPopup "shortenBody"
  , configPopupEllipsizeBody = (r 5 p nPopup "shortenBody") /= -1
  , configPopupDismissButton = r' "mouse1" p nPopup "dismissButton"
  , configPopupDefaultActionButton = r' "mouse3" p nPopup "defaultActionButton"

    -- buttons
  , configButtonsPerRow = r 5 p buttons "buttonsPerRow"
  , configButtonHeight = r 60 p buttons "buttonHeight"
  , configButtonMargin = r 2 p buttons "buttonMargin"
  , configLabels = r' "" p buttons "labels"
  , configCommands = r' "" p buttons "commands"

  }
  where nPopup = "notification-center-notification-popup"
        nCenter = "notification-center"
        colors = "colors"
        buttons = "buttons"
        r = readConfig
        r' = readConfig
        r'' = readConfig
        keys conditions = map (\c -> if length (splitted c) == 2
                                     then (splitted c) else ["", ""]) conditions
          where splitted condition = splitOn "=" condition

        match = map (matcherFunction) $ splitOn ";" <$>
                (split $ removeOuterLetters $ r' "" p nCenter "match")
                where lookupFun name noti = Text.unpack $ fromMaybe (Text.pack "")
                        ((lookup name
                          [ ("title", notiSummary)
                          , ("body", notiBody)
                          , ("app", notiAppName)
                          , ("time", notiTime) ]) <*> (Just noti))
                      matcherFunction conditions = \noti -> foldl (
                        \matches (k:v:[]) -> matches && ((v == lookupFun k noti)))
                                                      True (keys conditions)
        modify = map (matcherFunction) $ splitOn ";" <$>
                 (split $ removeOuterLetters $ r' "" p nCenter "modify")
          where matcherFunction condition = \noti -> foldl (
                  \noti (k:v:[]) -> switch k v noti) noti (keys condition)
                        where switch k v noti
                                | k == "title"
                                = noti { notiSummary = Text.pack v }
                                | k == "body"
                                = noti { notiBody = Text.pack v }
                                | k == "app"
                                = noti { notiAppName = Text.pack v }
                                | k == "time"
                                = noti { notiTime = Text.pack v }
                                | k == "timeout"
                                = noti { notiTimeout = read v :: Int32 }
                                | k == "right"
                                = noti { notiRight = Just . (read :: String -> Int) $ v }
                                | k == "top"
                                = noti { notiTop = Just . (read :: String -> Int) $ v }
                                | k == "icon"
                                = noti { notiIcon = parseImageString $ Text.pack v }
                                | k == "image"
                                = noti { notiImg = parseImageString $ Text.pack v }
                                | k == "transient" && v == "true"
                                = noti { notiTransient = True }
                                | k == "transient" && v == "false"
                                = noti { notiTransient = False }
                                | k == "noClosedMsg" && v == "true"
                                = noti { notiSendClosedMsg = False }
                                | k == "removeActions"
                                = noti { notiActions = [] }
                                | otherwise = noti

        run = [Nothing]
