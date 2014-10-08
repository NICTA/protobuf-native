{-# LANGUAGE OverloadedStrings #-}
module TextProtocolBuffers (pBench) where

import TestsPerson.Person
import TestsPerson.Name
import Text.ProtocolBuffers

pBench n = do
  let val = Person (Name (Just (fromString "Max")) Nothing) n (Just (fromString "maxwell.swadling@nicta.com.au"))
  if (Right (val, "") /= messageGet (messagePut val))
    then error "assert fail"
    else return ()
