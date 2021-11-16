---
menuItem: mi-docs
---

# Reach Script

The [reach](https://github.com/reach-sh/reach-lang/blob/master/reach) command-line script is the primary tool for Reach developers:

``` js nonum
reach help
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