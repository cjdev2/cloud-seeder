#!/usr/bin/env stack
-- stack runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Main

main :: IO ()
main = cliIO $ deployment "cloud-seeder-example" $ do
  stack_ "base"
  stack_ "server"
