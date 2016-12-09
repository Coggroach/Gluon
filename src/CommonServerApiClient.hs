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

handshakeApi :: Proxy handshakeApi
handshakeApi = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

shake :: Identity -> ClientM Int

echo :: String -> ClientM String

-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

(shake :<|> echo) =  client handshakeApi