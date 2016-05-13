module DB where

import Database.HDBC
import Database.HDBC.Sqlite3

connectDB :: IO Connection
connectDB = connectSqlite3 "unnumbers.sqlite3"
