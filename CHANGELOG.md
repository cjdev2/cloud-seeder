## 0.2.0.0 (April 19th, 2018)

### New Features

- Added `wait` command to wait until stack is in a stable state
- Added `teardown` command to destroy stack
- Logging now uses ToJSON instances to provide accurate and efficient YAML
- Stack policy can now be set in deployment configurations using `policy`
- CLI gives useful information about stacks when actions complete
- Already-provisioned stack parameters default to previously used values

### Bugfixes and Minor Changes

- Stability and error-handling improvements
- onCreate hooks run only when creating, rather than updating

## 0.1.0.0 (July 26th, 2017)

### Breaking Changes

- The `deploy` command is renamed to `provision`.
- The environment is now passed as a positional argument instead of as an environment variable.

### New Features

- Added support for configuring parameters with command-line flags and hardcoded environments, in addition to environment variables.
- Parameters are now parsed from template files to determine whether or not they are optional.
- Tags `cj:application` and `cj:environment` are always added, based on the mandatory `ENV` positional argument and information in the configuration.
- The `whenEnv` and `getEnvArg` functions are provided to conditionally configure stacks based on the current environment.

### Bugfixes and Minor Changes

- Various improvements to CLI help text.

## 0.0.0.0 (June 26th, 2017)

- Initial release
