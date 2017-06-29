# cloud-seeder [![Build Status](https://travis-ci.org/cjdev/cloud-seeder.svg?branch=master)](https://travis-ci.org/cjdev/cloud-seeder)

`cloud-seeder` is a Haskell library for interacting with CloudFormation stacks.

Let's say you have a CloudFormation template, in a file called `bucket.yaml`:

```yaml
AWSTemplateFormatVersion: '2010-09-09'
Description: cloud-seeder-example

Parameters:
  Env:
    Type: String
    Description: The environment in which to deploy
    MinLength: 1
  Foo:
    Type: String

Resources:
  Bucket:
    Type: AWS::S3::Bucket

Outputs:
  Bucket:
    Value: !Ref Bucket
```

You can create a deployment configuration in `deploy.hs`:

```haskell
#!/usr/bin/env stack
-- stack runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.CloudSeeder

main = cliIO $ deployment "cloud-seeder-example" $ do
  environment $ ["Foo"]
  stack_ "bucket"
```

Things to notice:

  - There's a shebang at the top of this file. Because of Stack's script interpreter support (options to which are passed in the second line), we can run it anywhere.

  - We need the GHC language extension `OverloadedStrings` to allow `String`-like representation of the `Text` datatype, which our tool uses under the hood.

  - The stack name must be the same as the template filename, minus `.yaml`.

  - `environment` lists any environment variables that must be provided to fill in the Parameters in the template...

  - ...however, `Env` is *always* required, as it is used to namespace stacks, i.e. `lab-app-server` vs `production-app-server`.

Run `env Env="test" Foo="bar" ./deploy.hs deploy --stack-name bucket`. Load up your CloudFormation console in AWS, and you should see a stack called `test-cloud-seeder-example-bucket` spinning up!
