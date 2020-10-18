
module Config
  (
    Config(..)
  , getConfig
  , replaceColors
  )where

import Data.List.Split (splitOn)
import Helpers (split, removeOuterLetters, readConfig, replace)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Int ( Int32, Int )
import NotificationCenter.Notifications.Data
  (notiSendClosedMsg, notiTransient, notiIcon, notiTime, notiAppName
  , notiBody, notiSummary, Notification(..))

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
  , configNotiCenterTimeTextSize :: String
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
  , configTitleTextSize :: String
  , configAppNameTextSize :: String
  , configTimeTextSize :: String
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
  , configUserButtonColor :: String
  , configUserButtonHover :: String
  , configUserButtonHoverColor :: String
  , configUserButtonBackground :: String
  , configUserButtonTextSize :: String
  , configUserButtonState1 :: String
  , configUserButtonState2 :: String
  , configUserButtonState1Color :: String
  , configUserButtonState2Color :: String
  , configUserButtonState1Hover :: String
  , configUserButtonState2Hover :: String
  , configUserButtonState1HoverColor :: String
  , configUserButtonState2HoverColor :: String

  -- colors
  , configBackground :: String
  , configBackgroundNoti :: String
  , configNotiLabelColor :: String
  , configCritical :: String
  , configCriticalInCenter :: String
  , configButtonColor :: String
  , configButtonHover :: String
  , configButtonHoverColor :: String
  , configButtonBackground :: String
  , configLabelColor :: String
  , configCriticalColor :: String
  , configCriticalInCenterColor :: String
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
  , configNotiCenterTimeTextSize = r' "32px" p nCenter "notiCenterTimeTextSize"
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
  , configTitleTextSize = r' "16px" p nPopup "titleTextSize"
  , configAppNameTextSize = r' "12px" p nPopup "appNameTextSize"
  , configTimeTextSize = r' "12px" p nPopup "timeTextSize"
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
  , configUserButtonColor = r' "#fee" p buttons "buttonColor"
  , configUserButtonHover = r' "rgba(0, 20, 20, 0.2)" p buttons "buttonHover"
  , configUserButtonHoverColor = r' "#fee" p buttons "buttonHoverColor"
  , configUserButtonBackground = r' "rgba(255, 255, 255, 0.15)" p buttons "buttonBackground"
  , configUserButtonTextSize = r' "12px" p buttons "buttonTextSize"
  , configUserButtonState1 = r' "rgba(255,255,255,0.5)" p buttons "buttonState1"
  , configUserButtonState2 = r' "rgba(255,255,255,0.3)" p buttons "buttonState2"
  , configUserButtonState1Color = r' "#fff" p buttons "buttonState1Color"
  , configUserButtonState2Color = r' "#fff" p buttons "buttonState2Color"
  , configUserButtonState1Hover = r' "rgba(0, 20, 20, 0.4)" p buttons "buttonState1Hover"
  , configUserButtonState2Hover = r' "rgba(0, 20, 20, 0.3)" p buttons "buttonState2Hover"
  , configUserButtonState1HoverColor = r' "#fee" p buttons "buttonState1HoverColor"
  , configUserButtonState2HoverColor = r' "#fee" p buttons "buttonState2HoverColor"

    -- colors
  , configBackground = r' "rgba(29, 27, 20, 0.6)" p colors "background"
  , configBackgroundNoti = r' "rgba(9, 0, 0, 0.5)" p colors "notiBackground"
  , configNotiLabelColor = r' "#fef3f6" p colors "notiColor"
  , configCritical = r' "rgba(255, 0, 0, 0.5)" p colors "critical"
  , configCriticalInCenter = r' "rgba(155, 0, 20, 0.5)" p colors "criticalInCenter"
  , configButtonColor = r' "#FFFFFF" p colors "buttonColor"
  , configButtonHover = r' "rgba(0, 20, 20, 0.2)" p colors "buttonHover"
  , configButtonHoverColor = r' "#fee" p colors "buttonHoverColor"
  , configButtonBackground = r' "transparent" p colors "buttonBackground"
  , configLabelColor = r' "#eae2e0" p colors "labelColor"
  , configCriticalColor = r' "#FFFFFF" p colors "criticalColor"
  , configCriticalInCenterColor = r' "#FFFFFF" p colors "criticalInCenterColor"
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
                                | k == "title" = noti { notiSummary = Text.pack v }
                                | k == "body" = noti { notiBody = Text.pack v }
                                | k == "app" = noti { notiAppName = Text.pack v }
                                | k == "time" = noti { notiTime = Text.pack v }
                                | k == "timeout" = noti { notiTimeout = read v :: Int32 }
                                | k == "right" = noti { notiRight = Just . (read :: String -> Int) $ v }
                                | k == "top" = noti { notiTop = Just . (read :: String -> Int) $ v }
--                                | k == "icon" = noti { notiIcon = Text.pack v }
                                | k == "transient" && v == "true" = noti { notiTransient = True }
                                | k == "transient" && v == "false" = noti { notiTransient = False }
                                | k == "noClosedMsg" && v == "true" = noti { notiSendClosedMsg = False }
                                | otherwise = noti

        run = [Nothing]


replaceColors config style =
  replace "replaceme0000" (configBackground config) $
  replace "replaceme0001" (configCritical config) $
  replace "replaceme0002" (configCriticalInCenter config) $
  replace "replaceme0003" (configButtonColor config) $
  replace "replaceme0004" (configButtonHover config) $
  replace "replaceme0005" (configButtonHoverColor config) $
  replace "replaceme0006" (configButtonBackground config) $
  replace "replaceme0007" (configLabelColor config) $
  replace "replaceme0008" (configCriticalColor config) $
  replace "replaceme0009" (configCriticalInCenterColor config) $
  replace "replaceme0010" (configBackgroundNoti config) $
  replace "replaceme0011" (configNotiLabelColor config) $
  replace "replaceme0012" (configUserButtonTextSize config) $
  replace "replaceme0013" (configUserButtonBackground config) $
  replace "replaceme0014" (configUserButtonColor config) $
  replace "replaceme0015" (configUserButtonHover config) $
  replace "replaceme0016" (configUserButtonHoverColor config) $
  replace "replaceme0017" (configUserButtonState1 config) $
  replace "replaceme0018" (configUserButtonState2 config) $
  replace "replaceme0019" (configUserButtonState1Color config) $
  replace "replaceme0020" (configUserButtonState2Color config) $
  replace "replaceme0021" (configUserButtonState1Hover config) $
  replace "replaceme0022" (configUserButtonState2Hover config) $
  replace "replaceme0023" (configUserButtonState1HoverColor config) $
  replace "replaceme0024" (configUserButtonState2HoverColor config) $
  replace "replaceme0025" (configTitleTextSize config) $
  replace "replaceme0026" (configAppNameTextSize config) $
  replace "replaceme0027" (configNotiCenterTimeTextSize config) $
  replace "replaceme0028" (configTimeTextSize config) style

