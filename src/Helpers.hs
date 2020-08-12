module Helpers where

import qualified Data.ConfigFile as CF
import qualified Control.Monad.Error as Error
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Functor (fmap)

import Text.Regex.TDFA
import qualified Data.Text as Text
import Data.Char ( chr )
import Text.I18N.GetText (textDomain, bindTextDomain, getText)
import System.Locale.SetLocale (setLocale, Category(LC_ALL))
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getExecutablePath)

initI18n = do
  setLocale LC_ALL (Just "")
  mypath <- getExecutablePath
  bindTextDomain "deadd-notification-center"
    (Just ((reverse $ removeFromLastSlash (reverse mypath))
            ++ "/../share/locale/"))
  textDomain (Just "deadd-notification-center")

removeFromLastSlash :: String -> String
removeFromLastSlash ('/':as) = as
removeFromLastSlash (a:as) = removeFromLastSlash as

translate :: String -> String
translate = unsafePerformIO . getText

readConfig :: CF.Get_C a => a -> CF.ConfigParser -> String -> String -> a
readConfig defaultVal conf sec opt = fromEither defaultVal
  $ fromEither (Right defaultVal) $ Error.runErrorT $ CF.get conf sec opt

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

markupify a = Text.pack $ markupify' $ Text.unpack a
markupify' :: String -> String
markupify' ('&':ls) = "&amp;" ++ (markupify' ls)
markupify' (l:ls) = l:(markupify' ls)
markupify' [] = []

atMay :: [a] -> Int -> Maybe a
atMay ls i = if length ls > i then
  Just $ ls !! i else Nothing

removeAllTags :: String -> String
removeAllTags text =
  let (a, _, c) =
        (text =~ "<(/a|i|/i|u|/u|b|/b)>|<(a|img)( +[^>]*)*>"
         :: (String, String, String))
  in a ++ if length c > 0 then removeAllTags c else ""


removeImgTag :: String -> (String, [(String, String)])
removeImgTag text =
  let (a, _, c, ms) =
        (text =~ "<img([^>]*)/>"
         :: (String, String, String, [String]))
  in ((a ++ (if length ms > 0 then fst $ removeImgTag c else c))
     , fromMaybe [] $ findTagProps <$> atMay ms 0)

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
              (text =~ "&#([0-9]{2,3});"
               :: (String, String, String, [String]))
            ascii = if length ms > 0 then (read $ head ms :: Int) else -1
            repl = if 32 <= ascii && ascii <= 126 
                   then [chr ascii] else matched
        in a ++ repl ++ (if length c > 0 then parseAsciiEntities c else "")
      parseNamedEntities text = 
        let (a, matched, c, ms) = 
              (text =~ "&([A-Za-z0-9]+);"
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
     

findTagProps :: String -> [(String, String)]
findTagProps match =
  let (_, _, rest, keys) = (match =~ "([^ =]+)=\"([^\"]*)\""
                             :: (String, String, String, [String]))
  in if length keys > 0 then [(keys !! 0, keys !! 1)] ++ (findTagProps rest) else []
