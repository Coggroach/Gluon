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

identityApi :: Proxy IdentityApi
identityApi = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

--identitySubmit :: Identity -> ClientM Response
--identityNext :: ServerType -> ClientM Identity
--identityAll :: ServerType -> ClientM [Identity]
--identityPort :: ServerType -> ClientM Int
--identityReport :: Identity -> ClientM Response

-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

--(identitySubmit :<|> identityNext :<|> identityAll :<|> identityPort :<|> identityReport) =  Servant.Client.client identityApi
