module NotificationCenter.Notifications.Data (
  Urgency(..)
  ) where

data Urgency = Normal | Low | High deriving Eq
