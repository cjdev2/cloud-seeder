#!/usr/bin/env stack
-- stack runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.CloudSeeder

main :: IO ()
main = deployment "cloud-seeder-example" $ do
  flag "BucketName"
  flag "BucketName2"
  stack_ "bucket"
