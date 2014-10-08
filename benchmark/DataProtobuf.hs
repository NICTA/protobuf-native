{-# LANGUAGE DeriveGeneric, DataKinds, OverloadedStrings #-}
module DataProtobuf (cBenchmark) where

import Person
import Data.Protobuf

cBenchmark n = do
  let val = Person (Name (Just "Max") Nothing) n (Just "maxwell.swadling@nicta.com.au")
  person <- newPb :: IO PersonPtr
  assign person val
  val2 <- derefPb person
  if (val /= val2)
    then error "assert fail"
    else return ()
