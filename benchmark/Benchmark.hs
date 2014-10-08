module Main where

-- This isn't a very good benchmark.

import Criterion.Main
import Test.QuickCheck

import DataProtobuf
import DataProtocolBuffers
import TextProtocolBuffers

main = defaultMain [
    bgroup "Data.Protobuf" [
      bench "some" $ whnfIO $ mapM cBenchmark [1..1000]
    , bench "lots" $ whnfIO $ mapM cBenchmark [1..10000]
    ]
  , bgroup "Data.ProtocolBuffers" [
      bench "some" $ whnfIO $ mapM protocolBuffers [1..1000]
    , bench "lots" $ whnfIO $ mapM protocolBuffers [1..10000]
    ]
  , bgroup "Text.ProtocolBuffers" [
      bench "some" $ whnfIO $ mapM pBench [1..1000]
    , bench "lots" $ whnfIO $ mapM pBench [1..10000]
    ]
  -- TODO: , bgroup "c++"
  ]
