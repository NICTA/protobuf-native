{-# LANGUAGE DeriveGeneric, DataKinds, OverloadedStrings #-}

module DataProtocolBuffers (protocolBuffers) where

import Data.Int
import Data.ProtocolBuffers
import Data.Serialize
import Data.Text
import GHC.Generics (Generic)
import GHC.TypeLits

data Name = Name
  { firstname :: Optional 1 (Value Text)
  , lastname :: Optional 2 (Value Text)
  } deriving (Generic, Show, Eq)

instance Encode Name
instance Decode Name

data Person = Person
  { name :: Required 1 (Message Name)
  , id :: Required 2 (Value Int32)
  , email :: Optional 3 (Value Text)
  } deriving (Generic, Show, Eq)

instance Encode Person
instance Decode Person

protocolBuffers n = do
  let name = Name { firstname = putField (Just "Max"), lastname = putField Nothing }
  let msg = Person
             { name = putField name
             , DataProtocolBuffers.id = putField n
             , email = putField (Just "maxwell.swadling@nicta.com.au")}
  if (Right msg /= runGet decodeMessage (runPut (encodeMessage msg)))
    then error "assert fail"
    else return ()
