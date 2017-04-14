{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module NotificationCenter.Notifications.Notification.Glade where

import Data.String.Here.Uninterpolated (hereFile)

glade =
  [hereFile|notification.glade|]
