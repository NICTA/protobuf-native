{-# LANGUAGE TemplateHaskell, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Main where

import Person
import Data.Protobuf

main :: IO ()
main = do
  let val = (Person (Name (Just "Max") Nothing) 1 (Just "maxwell.swadling@nicta.com.au"))
  person <- newPb :: IO PersonPtr
  assign person val
  writeProtobuf "person.pb" person
  
  person2 <- newPb :: IO PersonPtr
  readProtobuf "person.pb" person2
  val2 <- derefPb person2
  print $ val == val2
