{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import qualified Data.ConfigFile as CF
import qualified Control.Monad.Except as Error
import Control.Applicative ((<|>))
import Control.Monad
import Data.Foldable
import Data.Functor (fmap)
import Data.Gettext
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup (Tag(..), renderTags
                         , canonicalizeTags, parseTags, isTagCloseName)

import Text.Regex.TDFA
import qualified Data.Text as Text
import Data.Char ( chr )
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.Locale.SetLocale

import Paths_linux_notification_center

-- i18n related functions
getMoFile = do
  currentLocale <- fromMaybe "en" <$> setLocale LC_ALL (Just "")
  let textDomain = "deadd-notification-center"
  applicationDirectory <- takeDirectory . takeDirectory <$> getExecutablePath
  let localesDirectory = applicationDirectory </> "share" </> "locale"
  -- POSIX.1-2017, section 8.2 Internationalization Variables states the format
  -- is language[_territory][.codeset]. Since there are translations with
  -- territory specified, search for locales with reducing granularity.
  let paths =
        fmap
          (</> "LC_MESSAGES" </> textDomain <> ".mo")
          [ localesDirectory </> currentLocale
          , localesDirectory </> takeWhile (/= '.') currentLocale
          , localesDirectory </> takeWhile (/= '_') currentLocale
          ]
  pathsFromCabal <-
    mapM getDataFileName $
      fmap
        (</> "LC_MESSAGES" </> textDomain <> ".mo")
        [ "translation" </> currentLocale
        , "translation" </> takeWhile (/= '.') currentLocale
        , "translation" </> takeWhile (/= '_') currentLocale
        ]
  filterM doesFileExist pathsFromCabal >>= return . head

getCatalog = getMoFile >>= loadCatalog

readConfig :: CF.Get_C a => a -> CF.ConfigParser -> String -> String -> a
readConfig defaultVal conf sec opt = fromEither defaultVal
  $ fromEither (Right defaultVal) $ Error.runExceptT $ CF.get conf sec opt

readConfigFile :: String -> IO CF.ConfigParser
readConfigFile path = do
  c <- Error.catchError (CF.readfile CF.emptyCP{CF.optionxform = id} path)
    (\e ->  do
        putStrLn $ show e
        return $ return CF.emptyCP)
  let c1 = fromEither CF.emptyCP c
  return c1

fth (_, _, _, a) = a

fromEither :: a -> Either b a -> a
fromEither a e = case e of
  Left _ -> a
  Right x -> x

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing

replace a b c = replace' c a b
replace' :: Eq a => [a] -> [a] -> [a] -> [a]
replace' [] _ _ = []
replace' s find repl =
    if take (length find) s == find
        then repl ++ (replace' (drop (length find) s) find repl)
        else [head s] ++ (replace' (tail s) find repl)

-- split a string at ":"
split :: String -> [String]
split ('"':':':'"':ds) = "" : split ds
split (a:[]) = [[a]]
split (a:bs) = (a:(split bs !! 0)): (tail $ split bs)
split [] = []

splitOn :: Char -> String -> [String]
splitOn c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : splitOn c rest
  where (chunk, rest) = break (==c) s


trimFront :: String -> String
trimFront (' ':ss) = trimFront ss
trimFront ss = ss

trimBack :: String -> String
trimBack = reverse . trimFront . reverse

trim :: String -> String
trim = trimBack . trimFront

isPrefix :: String -> String -> Bool
isPrefix (a:pf) (b:s) = a == b && isPrefix pf s
isPrefix [] _ = True
isPrefix _ _ = False

removeOuterLetters (a:as) = reverse $ tail $ reverse as
removeOuterLetters [] = []


splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n as = (take n as) : (splitEvery n $ tailAt n as)
  where
    tailAt 0 as = as
    tailAt n (a:as) = tailAt (n - 1) as
    tailAt _ [] = []

atMay :: [a] -> Int -> Maybe a
atMay ls i = if length ls > i then
  Just $ ls !! i else Nothing


removeAllTags :: Text.Text -> Text.Text
removeAllTags = renderTags . (filterTags []) . canonicalizeTags . parseTags

-- The following tags should be supported:
-- <b> ... </b>              Bold
-- <i> ... </i>              Italic
-- <u> ... </u>              Underline
-- <a href="..."> ... </a>   Hyperlink
markupify :: Text.Text -> Text.Text
markupify = renderTags . (filterTags ["b", "i", "u", "a"])
  . canonicalizeTags . parseTags

filterTags :: [Text.Text] -> [Tag Text.Text] -> [Tag Text.Text]
filterTags supportedTags [] = []
filterTags supportedTags (tag : rest) = case tag of
  TagText _        -> keep
  TagOpen "img" _  -> process "img" skip
  TagOpen name _   ->
    let conversion = if isSupported name then enclose name else strip
    in process name conversion
  otherwise        -> next
  where
    isSupported name = elem name supportedTags

    keep = tag : next
    next = filterTags supportedTags rest

    skip _ = []
    strip  = filterTags supportedTags
    enclose name i = tag : (filterTags supportedTags i) ++ [TagClose name]

    process name conversion =
      let
        (inner, endTagRest) = break (isTagCloseName name) rest
      in (conversion inner) ++ (filterTags supportedTags endTagRest)


getImgTagAttrs :: Text.Text -> [(Text.Text, Text.Text)]
getImgTagAttrs text = getImg $ canonicalizeTags $ parseTags text
  where
    getImg [] = []
    getImg (tag : rest) = case tag of
      TagOpen "img" attr  -> attr
      otherwise        -> getImg rest



-- | Parses HTML Entities in the given string and replaces them with
-- their representative characters. Only operates on entities in
-- the ASCII Range. See the following for details:
--
-- <https://dev.w3.org/html5/html-author/charref>
-- <https://www.freeformatter.com/html-entities.html>
parseHtmlEntities :: String -> String
parseHtmlEntities =
  let parseAsciiEntities text =
        let (a, matched, c, ms) =
              (text =~ ("&#([0-9]{2,3});" :: String)
               :: (String, String, String, [String]))
            ascii = if length ms > 0 then (read $ head ms :: Int) else -1
            repl = if 32 <= ascii && ascii <= 126
                   then [chr ascii] else matched
        in a ++ repl ++ (if length c > 0 then parseAsciiEntities c else "")
      parseNamedEntities text =
        let (a, matched, c, ms) =
              (text =~ ("&([A-Za-z0-9]+);" :: String)
                :: (String, String, String, [String]))
            name = if length ms > 0 then head ms else ""
            repl = case name of
                     "quot" -> "\""
                     "apos" -> "'"
                     "grave" -> "`"
                     "amp" -> "&"
                     "tilde" -> "~"
                     "" -> matched
                     _ -> matched
        in a ++ repl ++ (if length c > 0 then parseNamedEntities c else "")
  in parseAsciiEntities . parseNamedEntities
