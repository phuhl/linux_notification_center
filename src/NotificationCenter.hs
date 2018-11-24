{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter where

import NotificationCenter.Notification
  (DisplayingNotificaton(..), showNotification, updateNoti)
import NotificationCenter.Notifications
  (NotifyState(..), startNotificationDaemon, Notification(..)
  , hideAllNotis)
import NotificationCenter.Glade (glade, style)
import NotificationCenter.Button
  (Button(..), createButton, setButtonState)
import TransparentWindow
import Helpers
import NotificationCenter.Notifications.Data

import Prelude

import Data.Int (Int32(..))
import Data.Tuple.Sequence (sequenceT)
import Data.Maybe
import Data.IORef
import Data.List
import Data.Time
import Data.Time.LocalTime
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Complex
import Data.Monoid ((<>))

import Control.Monad
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay, ThreadId(..))
import Control.Concurrent.STM
  ( readTVarIO, modifyTVar', TVar(..), atomically, newTVarIO )

import System.Locale.Read
import System.Posix.Signals (sigUSR1)
import System.Posix.Daemonize (serviced, daemonize)
import System.Directory (getHomeDirectory)

import DBus ( fromVariant )

import GI.Gtk
       (widgetSetHalign, widgetSetHexpand, buttonNew, setWidgetMargin, buttonSetRelief, widgetSetSizeRequest, widgetShowAll, widgetShow, widgetHide, onWidgetDestroy
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
import qualified GI.Gtk as Gtk (containerAdd, Window(..), Box(..), Label(..), Button(..))

import qualified GI.Gtk as GI (init, main)
import GI.GLib (sourceRemove, timeoutAdd, unixSignalAdd)
import GI.GLib.Constants
import GI.Gdk.Constants
import GI.Gdk.Flags (EventMask(..))
import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ShadowType(..)
       , PositionType(..), ReliefStyle(..), Align(..))
import Data.GI.Base.BasicConversions (gflagsToWord)
import qualified GI.Gdk.Objects.Window


data State = State
  { stMainWindow :: Gtk.Window
  , stNotiBox :: Gtk.Box
  , stTimeLabel :: Gtk.Label
  , stDateLabel :: Gtk.Label
  , stDeleteAll :: Gtk.Button
  , stUserButtons :: [ Button ]
  , stNotiState :: TVar NotifyState
  , stDisplayingNotiList :: [ DisplayingNotificaton ]
  , stNotisForMe :: [ Notification ]
  , stCenterShown :: Bool
  }


setTime :: TVar State -> IO Bool
setTime tState = do
  state <- readTVarIO tState
  now <- zonedTimeToLocalTime <$> getZonedTime
  zone <- System.Locale.Read.getCurrentLocale
  let format = Text.pack . flip (formatTime zone) now
  labelSetText (stTimeLabel state) $ format "%H:%M"
  labelSetText (stDateLabel state) $ format "%A, %x"
  return False

startSetTimeThread :: TVar State -> IO ()
startSetTimeThread tState = do
  runAfterDelay 1000 (startSetTimeThread' tState)
  return ()

startSetTimeThread' :: TVar State -> IO ()
startSetTimeThread' tState = do
  addSource (setTime tState)
  time <- fromIntegral <$> diffTimeToPicoseconds <$> utctDayTime <$> getCurrentTime
  let delay = (60 * 1000000) - ((ceiling (time / 1000000)) `mod` (1000000 * 60))
  runAfterDelay delay (startSetTimeThread' tState)
  return ()


createNotiCenter :: TVar State -> Config -> IO ()
createNotiCenter tState config = do
  objs <- createTransparentWindow (Text.pack glade)
    [ "main_window"
    , "label_time"
    , "label_date"
    , "box_notis"
    , "box_buttons"
    , "button_deleteAll" ]
    (Just "Notification area")

  mainWindow <- window objs "main_window"
  notiBox <- box objs "box_notis"
  buttonBox <- box objs "box_buttons"
  timeL <- label objs "label_time"
  timeD <- label objs "label_date"
  deleteButton <- button objs "button_deleteAll"

  screen <- windowGetScreen mainWindow
  setStyle screen $ BS.pack $
    replaceColors config style
--  (Just mainWindowGDK) <- widgetGetParentWindow mainWindow

  onButtonClicked deleteButton $ do
    displayList <- stDisplayingNotiList <$> readTVarIO tState
    mapM (removeNoti tState) displayList
    return ()


  let buttons = zip
        (split $ removeOuterLetters $ configLabels config)
        (split $ removeOuterLetters $ configCommands config)
      (buttonLabels, buttonCommands) = unzip buttons
      margin = fromIntegral $ configButtonMargin config
      width = fromIntegral (((configWidth config) - 20)
                            `div` (configButtonsPerRow config))
              - margin * 2
      height = fromIntegral $ configButtonHeight config
      linesNeeded = fromIntegral $ ceiling
        $ ((fromIntegral $ length buttons) / (fromIntegral $ configButtonsPerRow config))
  lines' <- sequence $ take linesNeeded $ repeat $ boxNew OrientationHorizontal 0
  buttons' <- sequence $ map
    (\(label, command) -> createButton config width height command label)
    buttons


  sequence $ map (\(box, buttons'') ->
                    sequence $ Gtk.containerAdd box <$> buttons'')
    $ zip (reverse lines') (splitEvery (configButtonsPerRow config)
                            $ (buttonButton <$> buttons'))
  sequence $ Gtk.containerAdd buttonBox <$> lines'
  sequence $ widgetShowAll <$> lines'

  atomically $ modifyTVar' tState $
    \state -> state { stMainWindow = mainWindow
                      , stNotiBox = notiBox
                      , stTimeLabel = timeL
                      , stDateLabel = timeD
                      , stDeleteAll = deleteButton
                      , stNotisForMe = []
                      , stUserButtons = buttons' }

  startSetTimeThread tState

  (screenH, screenW) <- getScreenProportions mainWindow

  onWidgetLeaveNotifyEvent mainWindow $ \(_) -> do
    hideNotiCenter tState
    return True

                                 -- w   h
  windowSetDefaultSize mainWindow (fromIntegral $ configWidth config) (screenH - barHeight)
  windowMove mainWindow (screenW - (fromIntegral $ configWidth config)) barHeight
  onWidgetDestroy mainWindow mainQuit
  return ()
    where barHeight = fromIntegral $ configBarHeight config

parseNotisForMe tState = do
  state <- readTVarIO tState
  -- do stuff
  let buttons = stUserButtons state
      notisForMe = stNotisForMe state
      myNotiHints = notiHints <$> notisForMe
      maybeIds = Map.lookup "id" <$> myNotiHints
      ids = join <$> map (\id' -> fromVariant <$> id') maybeIds :: [ Maybe Int32 ]
      ids' = map checkId ids
        where
          checkId (Just id)
            | id < (fromIntegral $ length buttons) = Just id
            | otherwise = Nothing
          checkId Nothing = Nothing
      maybeStates = Map.lookup "state" <$> myNotiHints
      states = join <$> map (\id' -> fromVariant <$> id') maybeStates :: [ Maybe Bool ]
      comb = sequenceT <$> (zip
                            (map
                             (\id -> (!!) buttons <$> (fromIntegral <$> id))
                             ids') states) :: [ Maybe (Button, Bool) ]
  sequence $ map (\comb' -> sequence $
                    (\(button, state) ->
                           setButtonState button state) <$> comb') comb
  atomically $ modifyTVar' tState
    (\state -> state { stNotisForMe = [] })
  return False

hideNotiCenter tState = do
  state <- readTVarIO tState
  mainWindow <- stMainWindow <$> readTVarIO tState
  widgetHide mainWindow
  atomically $ modifyTVar' tState
    (\state -> state { stCenterShown = False })


showNotiCenter tState notiState = do
  state <- readTVarIO tState
  mainWindow <- stMainWindow <$> readTVarIO tState
  newShown <- if stCenterShown state then
    do
      widgetHide mainWindow
      return False
    else
    do
      hideAllNotis $ stNotiState state
      widgetShow mainWindow
      return True
  atomically $ modifyTVar' tState
    (\state -> state {stCenterShown = newShown })
  return True

updateNotisForMe :: TVar State -> IO()
updateNotisForMe tState = do
  state <- readTVarIO tState
  notiState <- readTVarIO $ stNotiState state
  atomically $ modifyTVar' (stNotiState state) (
    \state -> state { notiForMeList = [] })
  atomically $ modifyTVar' tState (
    \state -> state { stNotisForMe = notiForMeList notiState
                                     ++ stNotisForMe state})

  addSource (parseNotisForMe tState)
  return ()

updateNotis :: TVar State -> IO()
updateNotis tState = do
  state <- readTVarIO tState
  notiState <- readTVarIO $ stNotiState state

  let newNotis = filter (
        \n -> (find (\nd -> dNotiId nd == notiId n )
               (stDisplayingNotiList state))
              == Nothing
              && (not $ notiTransient n)) $ notiStList notiState
  newNotis' <- mapM (
    \n -> do
      let newNoti = DisplayingNotificaton
                    { dNotiId = notiId n }
      showNotification (stNotiBox state) newNoti
        (stNotiState state) $ removeNoti tState
    ) newNotis

  let delNotis = filter (\nd -> (find (\n -> dNotiId nd == notiId n)
                                $ notiStList notiState) == Nothing)
                 $ stDisplayingNotiList state
  atomically $ modifyTVar' tState (
    \state -> state { stDisplayingNotiList =
                      newNotis' ++ stDisplayingNotiList state
                    })
  setDeleteAllState tState
  mapM (removeNoti tState) $ delNotis
  when (stCenterShown state) $
    do
      hideAllNotis $ stNotiState state
  return ()


removeNoti :: TVar State -> DisplayingNotificaton -> IO ()
removeNoti tState dNoti = do
  state <- readTVarIO tState
  atomically $ modifyTVar' tState $ \state ->
    state { stDisplayingNotiList =
            filter ((/=) dNoti) $ stDisplayingNotiList state}
  setDeleteAllState tState
  atomically $ modifyTVar' (stNotiState state) $ \state ->
    state { notiStList = filter
                         (\n -> notiId n /= dNotiId dNoti) $
                         notiStList state }
  addSource $ do
    dNotiDestroy dNoti
    return False
  return ()

setDeleteAllState tState = do
  addSource $ do
    state <- readTVarIO tState
    if (length $ stDisplayingNotiList state) > 1 then
      widgetShow $ stDeleteAll state
      else
      widgetHide $ stDeleteAll state
    return False
  return ()

getConfig p =
  Config
    {
    -- notification-center
      configBarHeight = r 0 p nCenter "marginTop"
    , configWidth = r 500 p nCenter "width"

    -- notification-center-notification-popup
    , configNotiDefaultTimeout = r 10000 p nPopup "notiDefaultTimeout"
    , configDistanceTop = r 50 p nPopup "distanceTop"
    , configDistanceRight = r 50 p nPopup "distanceRight"
    , configDistanceBetween = r 20 p nPopup "distanceBetween"
    , configWidthNoti = r 300 p nPopup "width"

    -- buttons
    , configButtonsPerRow = r 5 p buttons "buttonsPerRow"
    , configButtonHeight = r 60 p buttons "buttonHeight"
    , configButtonMargin = r 5 p buttons "buttonMargin"
    , configLabels = r' "" p buttons "labels"
    , configCommands = r' "" p buttons "commands"
    , configUserButtonColor = r' "#004" p buttons "buttonColor"
    , configUserButtonHover = r' "rgba(0, 20, 20, 0.2)" p buttons "buttonHover"
    , configUserButtonBackground = r' "rgba(100, 120, 120, 0.2)" p buttons "buttonBackground"
    , configUserButtonTextSize = r' "12px" p buttons "buttonTextSize"
    , configUserButtonActivated = r' "rgba(60, 80, 150, 0.4)" p buttons "buttonStatus1"
    , configUserButtonActivatedColor = r' "#fff" p buttons "buttonStatus1Color"

    -- colors
    , configBackground = r' "rgba(10, 50, 50, 0.8)" p colors "background"
    , configBackgroundNoti = r' "rgba(10, 50, 50, 0.8)" p colors "notiBackground"
    , configNotiLabelColor = r' "#FFFFFF" p colors "notiColor"
    , configCritical = r' "rgba(255, 0, 0, 0.2)" p colors "critical"
    , configCriticalInCenter = r' "rgba(100, 50, 50, 0.8)" p colors "criticalInCenter"
    , configButtonColor = r' "#FFFFFF" p colors "buttonColor"
    , configButtonHover = r' "darker(rgba(52, 50, 0, 0.8))" p colors "buttonHover"
    , configButtonBackground = r' "transparent" p colors "buttonBackground"
    , configLabelColor = r' "#FFFFFF" p colors "labelColor"
    , configCriticalColor = r' "#FFFFFF" p colors "criticalColor"
    , configCriticalInCenterColor = r' "#FFFFFF" p colors "criticalInCenterColor"
}
  where nPopup = "notification-center-notification-popup"
        nCenter = "notification-center"
        colors = "colors"
        buttons = "buttons"
        r = readConfig
        r' = readConfig

replaceColors config style =
  replace "replaceme0000" (configBackground config) $
  replace "replaceme0001" (configCritical config) $
  replace "replaceme0002" (configCriticalInCenter config) $
  replace "replaceme0003" (configButtonColor config) $
  replace "replaceme0004" (configButtonHover config) $
  replace "replaceme0005" (configButtonBackground config) $
  replace "replaceme0006" (configLabelColor config) $
  replace "replaceme0007" (configCriticalColor config) $
  replace "replaceme0008" (configCriticalInCenterColor config) $
  replace "replaceme0009" (configUserButtonColor config) $
  replace "replaceme0010" (configUserButtonHover config) $
  replace "replaceme0011" (configUserButtonBackground config) $
  replace "replaceme0012" (configUserButtonTextSize config) $
  replace "replaceme0013" (configUserButtonActivated config) $
  replace "replaceme0014" (configUserButtonActivatedColor config) $
  replace "replaceme0015" (configBackgroundNoti config) $
  replace "replaceme0016" (configNotiLabelColor config) style

getInitialState = do
  newTVarIO $ State
    { stDisplayingNotiList = []
    , stCenterShown = False}

main' :: IO ()
main' = do
  GI.init Nothing

  homeDir <- getHomeDirectory
  config <- getConfig <$> (readConfigFile
    (homeDir ++ "/.config/deadd/deadd.conf"))

  istate <- getInitialState
  notiState <- startNotificationDaemon config
    (updateNotis istate) (updateNotisForMe istate)

  atomically $ modifyTVar' istate $
    \istate' -> istate' { stNotiState = notiState }
  createNotiCenter istate config

  unixSignalAdd PRIORITY_HIGH (fromIntegral sigUSR1)
    (showNotiCenter istate notiState)

  GI.main

main :: IO ()
main = do
--  daemonize main'
  main'
