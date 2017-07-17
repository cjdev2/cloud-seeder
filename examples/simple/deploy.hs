#!/usr/bin/env stack
-- stack runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.CloudSeeder

main :: IO ()
main = cliIO $ deployment "cloud-seeder-example" $ do
  flag "BucketName"
  stack_ "bucket"
