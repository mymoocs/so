-- https://stackoverflow.com/questions/42711313/using-a-custom-datatype-in-a-aeson-record/42713012#42713012

-- {-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module SoJson where


import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString.Char8 (pack)

import GHC.Generics


import Data.Aeson.Types (Parser)
import Data.Text.Encoding (encodeUtf8)
-- import Servant
import Text.Email.Validate

{--type UserApi =
  "user" :> Get '[JSON] [User] :<|>
  "user" :> Capture "userId" Integer :> Get '[JSON] User

userServer :: Server UserApi
userServer =
  getUsers :<|>
  getUserById

getUsers :: Handler [User]
getUsers = return [exampleUser]

getUserById :: Integer -> Handler User
getUserById = \ case
  0 -> return exampleUser
  _ -> throwE err404

exampleUser :: User
exampleUser = User 0 "L. Smith" (fromJust (emailAddress "lsmith@example.com")) Base
--}
-- * user info
data UserLevel = Base | Admin
  deriving (Eq, Show, Generic)

instance FromJSON UserLevel
data User
  = User {
    userId :: Integer,
    userName :: String,
    userEmail :: EmailAddress,
    userLevel :: UserLevel
  }
  deriving (Eq, Show, Generic)

instance ToJSON User where
    toJSON (User userId userName userEmail userLevel) =
      object ["userId" .= userId, "userName" .= userName, "userEmail" .= show userEmail, "userLevel" .= show userLevel]
instance FromJSON EmailAddress where
    parseJSON = withText "EmailAddress" $ \t ->
                    case validate $ encodeUtf8 t of
                        Left err -> fail $ "Failed to parse email address: " ++ err
                        Right email -> return email

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    userId <- o .: "userId"
    userName <- o .: "userName"
    userEmail <- o .: "age"
    userLevel <- o .: "userLevel"
    return User{..}

{--instance FromJSON User where
  parseJSON (Object o) =  --"user" $ \o -> do
   User <$>
    o .: "userId" <*>
    o .: "userName" <*>
    o .: "age" <*>
    o .: "userLevel"
--}
