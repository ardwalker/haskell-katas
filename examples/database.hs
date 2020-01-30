
-- Mongo example 
-- https://hackage.haskell.org/package/mongoDB-2.3.0/docs/Database-MongoDB.html
-- https://hackage.haskell.org/package/persistent-mongoDB-2.1.1/docs/Database-Persist-MongoDB.html


-- Run it in the background
-- brew services start mongodb

-- Run mongo in the foreground 
-- mongod --config /usr/local/etc/mongod.conf

-- Using HDBC and HDBC-sqlite3 packages, you can connect and query it like this:

import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3

main = do conn <- connectSqlite3 "test.db"
          rows <- quickQuery' conn "SELECT * from t1" []
          forM_ rows $ \row -> putStrLn $ show row

