# cloud-seeder [![Build Status](https://travis-ci.org/cjdev/cloud-seeder.svg?branch=master)](https://travis-ci.org/cjdev/cloud-seeder)

`cloud-seeder` is a Haskell DSL for provisioning and controlling [CloudFormation][aws-cloudformation] stacks. It provides an opinionated mechanism for provisioning a set of related stacks called a “deployment”. You write ordinary CloudFormation [templates][aws-cloudformation-templates] as YAML, and `cloud-seeder` helps to create a self-executing command-line interface to orchestrate their deployment.

For example consider a template that provisions an S3 bucket with a configurable name, `bucket.yaml`:

```yaml
AWSTemplateFormatVersion: '2010-09-09'

Parameters:
  BucketName:
    Type: String

Resources:
  Bucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Ref BucketName

Outputs:
  Bucket:
    Value: !Ref Bucket
  BucketDomain:
    Value: !GetAtt Bucket.DomainName
```

Using `cloud-seeder`, you can create a deployment script in the same directory, `deploy.hs`:

```haskell
#!/usr/bin/env stack
-- stack runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.CloudSeeder

main = cliIO $ deployment "cloud-seeder-example" $ do
  stack "bucket" $ do
    flag "BucketName"
```

This file contains a declarative configuration of your deployment, but it also serves as an executable command-line tool! Since it has a shebang at the top, it can be used directly to run the deployment with a pleasant interface:

```
$ ./deploy.hs deploy bucket production --BucketName my-awesome-s3-bucket
```

The first argument to the `deploy` command is the stack you want to provision, and the second argument is the name of some “environment” to deploy into. This environment is used to namespace the eventual stack name, so the above command will spin up a new CloudFormation stack called `production-cloud-seeder-example-bucket`. The environment is also available in templates themselves if they specify an `Env` [parameter][aws-cloudformation-parameters]. The generated command-line interface is also robust in the face of mistakes, and it won’t do anything if a required parameter isn’t specified.

While `cloud-seeder` *can* be used for single-stack deployments, it’s far more useful when used with multiple stacks at a time, which may possibly depend on other stacks’ [outputs][aws-cloudformation-outputs]. For example, we may now wish to serve resources out of our S3 bucket by using CloudFront. We can write a second template to do the job, `cdn.yaml`:

```yaml
AWSTemplateFormatVersion: '2010-09-09'

Parameters:
  BucketDomainName:
    Type: String

Resources:
  Distribution:
    Type: AWS::CloudFront::Distribution
    Properties:
      DistributionConfig:
        Enabled: true
        Origins:
        - DomainName: !Ref BucketDomainName
          Id: origin
          S3OriginConfig: {}
        # ...
```

We can now add `cdn` to our deployment configuration:

```haskell
#!/usr/bin/env stack
-- stack runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.CloudSeeder

main = cliIO $ deployment "cloud-seeder-example" $ do
  stack "bucket" $ do
    flag "BucketName"

  stack_ "cdn"
```

We use `stack_` instead of `stack` to omit the configuration block, since the `cdn` stack doesn’t need any additional configuration options. When we go to deploy the stack, it will work just fine:

```
$ ./deploy.hs deploy cdn production
```

Note that we did **not** have to specify the `BucketDomainName` [parameter][aws-cloudformation-parameters] explicitly, because it was an [output][aws-cloudformation-outputs] from the `bucket` stack, so it is automatically passed downward to the `cdn` stack. This allows stacks defined lower in the configuration to build on top of resources defined in previous ones.

For more information about all of the configuration options available, as well as some of the implementation details, see [the documentation on Hackage][cloud-seeder].

[aws-cloudformation]: https://aws.amazon.com/cloudformation/
[aws-cloudformation-outputs]: http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/outputs-section-structure.html
[aws-cloudformation-parameters]: http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html
[aws-cloudformation-templates]: http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-guide.html
[cloud-seeder]: http://hackage.haskell.org/package/cloud-seeder
