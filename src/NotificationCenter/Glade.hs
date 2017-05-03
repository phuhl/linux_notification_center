{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module NotificationCenter.Glade where

import Data.String.Here.Uninterpolated (hereFile)

glade =
  [hereFile|notification_center.glade|]

style = [hereFile|style.css|]
