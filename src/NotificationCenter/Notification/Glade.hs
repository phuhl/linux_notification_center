{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module NotificationCenter.Notification.Glade (glade) where

import Data.String.Here.Uninterpolated (hereFile)

glade =
  [hereFile|notification_in_center.glade|]
