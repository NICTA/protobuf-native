module Main where

import Data.Protobuf
import Person

import Data.ByteString
import Test.QuickCheck
import Test.QuickCheck.Monadic

main = quickCheckWith stdArgs { maxSuccess = 1000 } $ monadicIO $ do
  b <- pick arbitrary :: PropertyM IO PersonT
  testProtobuf b
  b <- pick arbitrary :: PropertyM IO NameT
  testProtobuf b

testProtobuf b = do
  x <- run $ do
    p <- newPb
    assign p b
    v <- derefPb p
    return $ b == v
  assert x

instance Arbitrary ByteString where
  arbitrary = fmap pack arbitrary

instance Arbitrary NameT where
  arbitrary = Name <$> arbitrary <*> arbitrary

instance Arbitrary PersonT where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary
