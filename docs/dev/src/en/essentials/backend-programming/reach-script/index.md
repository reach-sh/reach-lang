---
menuItem: mi-docs
---

# Reach Script

The [reach](https://github.com/reach-sh/reach-lang/blob/master/reach) command-line script is the primary tool for Reach developers:

``` nonum
$ reach help
reach 0.1.6 (66f7fd96) - Reach command-line tool

Usage: reach COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  config                   Configure default Reach settings
  compile                  Compile an app
  clean                    Delete 'build/$MODULE.$IDENT.mjs'
  init                     Set up source files for a simple app in the current
                           directory
  run                      Run a simple app
  down                     Halt all Dockerized Reach services and devnets
  scaffold                 Set up Docker scaffolding for a simple app in the
                           current directory
  react                    Run a simple React app
  rpc-server               Run a simple Reach RPC server
  rpc-run                  Run an RPC server + frontend with development
                           configuration
  devnet                   Run only the devnet
  upgrade                  Upgrade Reach
  update                   Update Reach Docker images
  docker-reset             Kill and remove all Docker containers
  version                  Display version
  hashes                   Display git hashes used to build each Docker image
  help                     Show usage
```

# reach clean

``` nonum
$ reach clean -h
Usage: reach clean [MODULE] [IDENT]
  Delete 'build/$MODULE.$IDENT.mjs'

Available options:
  -h,--help                Show this help text

MODULE is "index" by default
IDENT  is "main"  by default

If:
 * MODULE is a directory then `cd $MODULE && rm -f "build/index.$IDENT.mjs";
 * MODULE is <something-else> then `rm -f "build/$MODULE.$IDENT.mjs"
```

# reach compile

``` nonum
$ reach compile -h
Usage: reach compile [-o|--output DIR] [SOURCE] [EXPORTS...] 
                     [--intermediate-files] [--install-pkgs] [--stop-after-eval]
                     [--verify-timeout TIMEOUT-MS]
  Compile an app

Available options:
  -o,--output DIR          Directory for output files
  --intermediate-files     Store intermediate files in output DIR
  --install-pkgs           Allow Reach to fetch remote package imports
  --stop-after-eval        Stop compilation process after evaluation
  --verify-timeout TIMEOUT-MS
                           Timeout per verification theorem in
                           milliseconds (default: 120000)
  -h,--help                Show this help text
```

# reach config

``` nonum
$ reach config -h
Usage: reach config [-v|--verbose]
  Configure default Reach settings

Available options:
  -v,--verbose             Print additional config info to `stdout`
  -h,--help                Show this help text
```

# reach devnet

``` nonum
$ reach devnet -h
Usage: reach devnet [--await-background]
  Run only the devnet

Available options:
  --await-background       Run in background and await availability
  -h,--help                Show this help text
```

# reach docker-reset

``` nonum
$ reach docker-reset -h
Usage: reach docker-reset [-y|--even-non-reach]
  Kill and remove all Docker containers

Available options:
  -y,--even-non-reach      Acknowledge non-interactively that ALL containers
                           will be halted
  -h,--help                Show this help text
```

# reach down

``` nonum
$ reach down -h
Usage: reach down 
  Halt all Dockerized Reach services and devnets

Available options:
  -h,--help                Show this help text
```

# reach hashes

``` nonum
$ reach hashes -h
Usage: reach hashes 
  Display git hashes used to build each Docker image

Available options:
  -h,--help                Show this help text
```

# reach init

``` nonum
$ reach init [TEMPLATE]
  Set up source files for a simple app in the current directory

Available options:
  -h,--help                Show this help text

Available templates:
  - default

Aborts if index.rsh or index.mjs already exist
```

# reach react

``` nonum
$ reach react -h
Usage: reach react [-o|--output DIR] [SOURCE] [EXPORTS...] 
                   [--intermediate-files] [--install-pkgs] [--stop-after-eval] 
                   [--verify-timeout TIMEOUT-MS]
  Run a simple React app

Available options:
  -o,--output DIR          Directory for output files
  --intermediate-files     Store intermediate files in output DIR
  --install-pkgs           Allow Reach to fetch remote package imports
  --stop-after-eval        Stop compilation process after evaluation
  --verify-timeout TIMEOUT-MS
                           Timeout per verification theorem in
                           milliseconds (default: 120000)
  -h,--help                Show this help text
```

# reach rpc-run

``` nonum
$ reach rpc-run -h
Usage: reach rpc-run EXECUTABLE [ARGS]
  Run an RPC server + frontend with development configuration

Available options:
  ARGS                     Zero or more arguments to be passed into EXECUTABLE
  -h,--help                Show this help text

Example:
 $ reach rpc-run python3 -u ./index.py
```

# reach rpc-server

``` nonum
$ reach rpc-server -h
Usage: reach rpc-server 
  Run a simple Reach RPC server

Available options:
  -h,--help                Show this help text
```

# reach run

``` nonum
$ reach run -h
Usage: reach run [APP or DIR] [ARGS]
  Run a simple app

Available options:
  APP or DIR               May be either a module name without its extension
                           (e.g. "index") or a relative sub-directory path
  ARGS                     Zero or more arguments to be passed into APP
  -h,--help                Show this help text
```

# reach scaffold

``` nonum
$ reach scaffold -h
Usage: reach scaffold [--quiet]
  Set up Docker scaffolding for a simple app in the current directory

Available options:
  --quiet                  Withhold progress messages
  -h,--help                Show this help text
```

# reach update

``` nonum
$ reach update -h
Usage: reach update 
  Update Reach Docker images

Available options:
  -h,--help                Show this help text
```

# reach upgrade

``` nonum
$ reach upgrade -h
Usage: reach upgrade 
  Upgrade Reach

Available options:
  -h,--help                Show this help text
```

# reach version

``` nonum
$ reach version 
  Display version

Available options:
  -h,--help                Show this help text
```
