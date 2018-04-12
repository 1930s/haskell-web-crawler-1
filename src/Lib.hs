{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , crawl
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.List
import           GHC.Generics
import           Network.HTTP.Conduit




import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad
import           Helper

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Section = Section
             { index :: String
             , line  :: String
             } deriving (Show, Generic)

data Link = Link
          { name :: String
          } deriving (Show, Generic)

instance FromJSON Section where
  parseJSON = withObject "Section" $ \v ->
    Section <$> v .: "index" <*> v .: "line"

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v ->
    Link <$> v .: "*"

data Parse = Sections [Section]
           | Links [Link]
            deriving (Show, Generic)

instance FromJSON Parse where
  parseJSON = withObject "Parse" $ \v ->
    (Sections <$> v .: "sections") <|> (Links <$> v .: "links")

data Root = Root Parse
          deriving (Show, Generic)

instance FromJSON Root where
  parseJSON = withObject "Root" $ \v ->
    Root <$> v .: "parse"



getSections :: String -> IO (Maybe Root)
getSections s = decode <$> simpleHttp url
  where url = "https://en.wikipedia.org/w/api.php?action=parse&prop=sections&format=json&page=" ++ s

getLinks :: String -> String -> IO (Maybe Root)
getLinks s i = decode <$> simpleHttp url
  where url = "https://en.wikipedia.org/w/api.php?action=parse&prop=links&format=json&page=" ++ s ++ "&section=" ++ i

getIndex :: Root -> Maybe String
getIndex (Root (Sections s)) = index <$> section
  where section = find (\(Section _ l) -> l == "See also") s
getIndex _ = Nothing

getNames :: Root -> Maybe [String]
getNames (Root (Links l)) = Just $ map name l
getNames _                = Nothing


crawl :: String -> Int -> Maybe Int -> Bool -> IO ()
crawl s n limitOption printOption = do
   (i, o) <- threadPoolIO n f
   writeChan i s
   loop i o 0 limitOption printOption
   where
    f a = do
      indexIO <- getSections a
      case indexIO >>= getIndex of
        Nothing -> return []
        Just i -> do
          namesIO <- getLinks a i
          case namesIO >>= getNames of
            Nothing -> return []
            Just l  -> return l

    loop i o n limitOption printOption = do
      if printOption
        then print n
        else return ()
      case limitOption of
        Nothing -> x
        Just end ->
          if n > end
          then return ()
          else x
      where x = do
              names <- readChan o
              sequence $ map (writeChan i) names
              if not printOption
                then sequence_ $ map print names
                else return ()
              loop i o (n + length names) limitOption printOption




