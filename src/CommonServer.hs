{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServer where


------------------------------
--  File Structure
------------------------------
data File = File { 
    fileName :: FilePath, 
    fileContent :: String 
} deriving (Eq, Show, Generic)

instance ToJSON File
instance FromJSON File

------------------------------
--  Server Identity 
------------------------------
data Identity = Identity { 
    address :: String, 
    port :: String 
} deriving (Eq, Show, Generic)

instance ToJSON Identity
instance FromJSON Identity

------------------------------
--  Response Packet 
------------------------------
data Response = Response { 
    code :: Int, 
    server :: Identity 
} deriving (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

------------------------------
--  Response Codes 
------------------------------
data ResponseCodes = 
    FileUploadComplete |
    FileUploadError |
    HandshakeSuccessful |
    HandshakeError
    