module Helpers where

import qualified Data.ConfigFile as CF
import qualified Control.Monad.Error as Error
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Functor (fmap)
--import Data.Sequence (drop)

import qualified Data.Text as Text
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

fromEither :: a -> Either b a -> a
fromEither a e = case e of
  Left _ -> a
  Right x -> x


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
