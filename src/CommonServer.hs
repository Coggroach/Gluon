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
--  Resources Directory 
------------------------------
data Resources = Resources { 
    path :: String     
} deriving (Eq, Show, Generic)

instance ToJSON Resources
instance FromJSON Resources

------------------------------
--  Response Packet 
------------------------------
data Response = Response { 
    code :: ResponseCode, 
    server :: Identity 
} deriving (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

------------------------------
--  Response Codes 
------------------------------
data ResponseCode = 
    FileUploadComplete |
    FileUploadError |
    HandshakeSuccessful |
    HandshakeError
