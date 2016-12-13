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
import Control.Monad.Trans.Either

identityApi :: Proxy IdentityApi
identityApi = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

identitySubmit :: Identity -> EitherT ServantError IO Response
identityNext :: ServerType -> EitherT ServantError IO Identity
identityAll :: ServerType -> EitherT ServantError IO [Identity]
identityPort :: ServerType ->EitherT ServantError IO Int
identityReport :: Identity -> EitherT ServantError IO Response

-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

identitySubmit :<|> identityNext :<|> identityAll :<|> identityPort :<|> identityReport =  Servant.Client.client identityApi (Servant.Client.BaseUrl Servant.Client.Http "localhost" 8081)


