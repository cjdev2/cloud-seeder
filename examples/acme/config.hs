#!/usr/bin/env stack
-- stack runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.CloudSeeder

main :: IO ()
main = deployment "cloud-seeder-example" $ do
  stack_ "base"
  stack "acme" $ do
    flag "OAuthPublicClientId"
    flag "OAuthConfidentialClientId"
    flag "OAuthConfidentialClientSecret"
    flag "ApplicationImageTag"
