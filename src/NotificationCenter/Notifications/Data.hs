{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications.Data
  ( Urgency(..)
  , CloseType (..)
  , Notification(..)
  , Image(..)
  , parseImageString
  , rawImgToPixBuf
  ) where

import qualified Data.Text as Text
import Data.Word ( Word32, Word8 )
import Data.Int ( Int32, Int )
import qualified Data.ByteString as BS
import Foreign.Marshal.Array (newArray)
import Foreign.C.Types (CUChar(..))
import Foreign.Ptr (Ptr)
import qualified Data.Map as Map ( Map )
import Data.List ( sortOn )
import DBus ( Variant (..), Signal )

import qualified Data.Yaml as Y
import Data.Yaml as Y ((.=))

import GI.GdkPixbuf (Pixbuf(..), pixbufNewFromData, Colorspace(..))

data Urgency = Normal | Low | High deriving Eq
data CloseType = Timeout | User | CloseByCall | Other deriving Eq

instance Eq Notification where
  a == b = notiId a == notiId b

data Notification = Notification
  { notiAppName :: Text.Text -- ^ Application name
  , notiRepId :: Word32 -- ^ Replaces id
  , notiId :: Int -- ^ Id
  , notiIcon :: Image -- ^ App icon
  , notiImg :: Image -- ^ Image
  , notiSummary :: Text.Text -- ^ Summary
  , notiBody :: Text.Text -- ^ Body
  , notiActions :: [Text.Text] -- ^ Actions
  , notiActionIcons :: Bool -- ^ Use icons for action-buttons
  , notiHints :: Map.Map Text.Text Variant -- ^ Hints
  , notiUrgency :: Urgency
  , notiTimeout :: Int32 -- ^ Expires timeout (milliseconds)
  , notiTime :: Text.Text
  , notiTransient :: Bool
  , notiSendClosedMsg :: Bool -- ^ If notiOnClosed should be ignored
  , notiOnClosed :: CloseType -> IO ()
  , notiTop :: Maybe Int
  , notiRight :: Maybe Int
    -- ^ Should be called when the notification is closed, either by
    --   timeout or by user
  , notiOnAction :: String -> Maybe String -> IO ()
    -- ^ Should be called when an action is used
  , notiPercentage :: Maybe Double
    -- ^ The percentage that should be shown in a percentage bar
  }

instance Y.ToJSON Notification where
  toJSON n = Y.object [
    "appname" .= notiAppName n
    , "repId" .= notiRepId n
    , "id" .= notiId n
    , "icon" .= ((show $ notiIcon n) :: String)
    , "img" .= ((show $ notiImg n) :: String)
    , "title" .= notiSummary n
    , "body" .= notiBody n
    , "actions" .= notiActions n
    , "actionIcons" .= notiActionIcons n
--    , "notiHints" .= notiHints n
--    , "notiUrgency" .= ((show $ notiUrgency n) :: String)
    , "timeout" .= notiTimeout n
    , "time" .= notiTime n
    , "transient" .= notiTransient n
    , "sendClosedMsg" .= notiSendClosedMsg n
    , "top" .= notiTop n
    , "right" .= notiRight n
    , "percentage" .= notiPercentage n ]

instance Show Notification where
  show n = foldl (++) ""
    [ "Notification { \n"
    , "  notiAppName = " ++ (Text.unpack $ notiAppName n) ++ ", \n"
    , "  notiRepId = " ++ (show $ notiRepId n) ++ ", \n"
    , "  notiId = " ++ (show $ notiId n) ++ ", \n"
    , "  notiIcon = " ++ (show $ notiIcon n) ++ ", \n"
    , "  notiImg = " ++ (show $ notiImg n) ++ ", \n"
    , "  notiSummary = " ++ (Text.unpack $ notiSummary n) ++ ", \n"
    , "  notiBody = " ++ (Text.unpack $ notiBody n) ++ ", \n"
    , "  notiActions = " ++ (show $ Text.unpack <$> notiActions n) ++ ", \n"
    , "  notiActionIcons = " ++ (show $ notiActionIcons n) ++ ", \n"
    , "  notiHints = " ++ (show $ notiHints n) ++ ", \n"
    , "  notiTimeout = " ++ (show $ notiTimeout n) ++ ", \n"
    , "  notiTime = " ++ (Text.unpack $ notiTime n) ++ ", \n"
    , "  notiTransient = " ++ (show $ notiTransient n) ++ ", \n"
    , "  notiSendClosedMsg = " ++ (show $ notiSendClosedMsg n) ++ "\n"
    , "  notiTop = " ++ (show $ notiTop n) ++ "\n"
    , "  notiRight = " ++ (show $ notiRight n) ++ "\n"
    , "  notiPercentage = " ++ (show $ notiPercentage n) ++ "\n"
    , " }\n" ]

data Image = RawImg
  ( Int32 -- width
  , Int32 -- height
  , Int32 -- rowstride
  , Bool -- alpha
  , Int32 -- bits per sample
  , Int32 -- channels
  , BS.ByteString -- image data
  )
  | ImagePath String
  | NamedIcon String
  | NoImage deriving Show

parseImageString :: Text.Text -> Image
parseImageString a = if (Text.isPrefixOf "file://" a) then
                       ImagePath $ Text.unpack$ Text.drop 6 a
                     else
                       if (Text.length a > 0) then
                         NamedIcon $ Text.unpack a
                       else
                         NoImage


rawImgToPixBuf :: Image -> IO Pixbuf
rawImgToPixBuf (RawImg (width, height, rowstride, alpha, bits, channels, datas))
  = do datas' <- newArray $ BS.unpack datas
       pixbufNewFromData datas'
         ColorspaceRgb alpha bits width height rowstride Nothing
