{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module CommonServerApiClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           CommonServerApi
import           CommonServer
import Network.HTTP.Client hiding(Proxy, Response)

identityApi :: Proxy IdentityApi
identityApi = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

idsubmit :: Identity -> ClientM Response
idnext :: ServerType -> ClientM Identity
idall :: ServerType ->  ClientM [Identity]
idport :: ServerType -> ClientM Int
idreport :: Identity -> ClientM Response

-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

idsubmit :<|> idnext :<|> idall :<|> idport :<|> idreport =  Servant.Client.client identityApi

fsfiles :: ClientM [FilePath]
fsdownload :: String ->  ClientM File
fsupload :: File -> ClientM Response

fsfiles :<|> fsdownload :<|> fsupload = Servant.Client.client fileApi


