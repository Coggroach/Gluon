{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MongoDbConnector where

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Logger

withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  Database.MongoDB.close pipe
  return ret

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" Prelude.id "127.0.0.1" True

mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False

mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" Prelude.id "USEHASKELLDB" True

defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> def