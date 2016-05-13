{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Snap.Core
import           Snap.Http.Server
import           UNNumber
import           Data.ByteString.Char8
import           Data.Maybe
import           Data.ByteString.Lazy (toStrict)
import           Text.Read (readMaybe)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [("unnumber/:unnumber", getUnNumber)]

getUnNumber :: Snap ()
getUnNumber = do
    param <- getParam "unnumber"
    if param == Nothing then
        writeBS "{\"error\" : \"Missing UN number\"}"
    else
        do
         let number = (readMaybe . unpack . fromJust) param
         if number == Nothing then
            writeBS "{\"error\" : \"Invalid number\"}"
         else
            do
                res <- liftIO $ lookUpUNNumberJSON number
                writeBS . toStrict $ res
