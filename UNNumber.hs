{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module UNNumber where

import Data.Aeson
import Data.Text hiding (head, length)
import Database.HDBC
import GHC.Generics
import Data.ByteString.Lazy hiding (head, length)

import DB

data UNNumber = UNNumber {unNumber :: Int, unClass :: !Text, unDescription :: !Text} deriving (Show, Generic)

instance FromJSON UNNumber
instance ToJSON UNNumber

rowToUNNumber :: [SqlValue] -> UNNumber
rowToUNNumber result = UNNumber {unNumber = number, unClass = unC, unDescription = unDesc}
                       where number = fromSql $ result !! 0
                             unC    = fromSql $ result !! 1
                             unDesc = fromSql $ result !! 2

lookUpUNNumber :: Int -> IO (Maybe UNNumber)
lookUpUNNumber number = do
    conn <- connectDB
    result <- quickQuery' conn "SELECT * from UNNumbers where number = ?" [toSql number]
    let unNumber = if length result > 0 then
                    Just $ rowToUNNumber (head result)
                   else
                    Nothing
    disconnect conn
    return unNumber

unNumberToJSON :: Maybe UNNumber -> ByteString
unNumberToJSON Nothing = "{\"error\" : \"Cannot find UN number\"}"
unNumberToJSON (Just number) = encode number

lookUpUNNumberJSON :: Int -> IO ByteString
lookUpUNNumberJSON number = do
    number <- lookUpUNNumber number
    return $ unNumberToJSON number
