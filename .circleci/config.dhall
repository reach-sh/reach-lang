let Prelude =
  https://prelude.dhall-lang.org/v20.1.0/package.dhall
  sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let Map    = Prelude.Map.Type
let map    = Prelude.List.map
let filter = Prelude.List.filter
let not    = Prelude.Bool.not

let MAJOR         = env:MAJOR   as Text
let MINOR         = env:MINOR   as Text
let PATCH         = env:PATCH   as Text
let VERSION       = env:VERSION as Text
let VERSION_SHORT = "${MAJOR}.${MINOR}"

let E         = ./examples.dhall
let examples  = E.examples
let Connector = E.Connector

let Connector/show = \(c : Connector) -> merge
  { ALGO = "ALGO"
  , CFX  = "CFX"
  , ETH  = "ETH"
  } c


let KeyVal
   = \(K : Type)
  -> \(V : Type)
  -> { mapKey : K, mapValue : V }

let `:=`
   = \(V : Type)
  -> \(k : Text)
  -> \(v : V)
  -> { mapKey = k, mapValue = v }


--------------------------------------------------------------------------------

let docker-image
   = \(i : Text)
  -> \(v : Text)
  -> "reachsh/${i}:${v}"

let docker-tag1
   = \(i : Text)
  -> \(o : Text)
  -> "docker tag ${i} ${o}"

let docker-tag
   = \(i : Text)
  -> \(v : Text)
  -> docker-tag1 (docker-image i "latest") (docker-image i v)

let docker-tag-all
  = \(i : Text) -> docker-tag i "${MAJOR}.${MINOR}.${PATCH}"
        ++ "\n" ++ docker-tag i "${MAJOR}.${MINOR}"
        ++ "\n" ++ docker-tag i "${MAJOR}"

let cimg-base = "cimg/base:stable-18.04"

let docker-creds =
  { auth = { username = "$DOCKER_LOGIN"
           , password = "$DOCKER_PASSWORD"
           }}


let default-docker-image =
  { image = cimg-base } /\ docker-creds


--------------------------------------------------------------------------------

let Run =
  let T =
    { name              : Text
    , command           : Text
    , no_output_timeout : Text
    , environment       : Map Text Text
    }

  let environment =
    [ `:=` Text "LC_ALL" "en_US.UTF-8" ]

  in { Type = T, default = { environment, no_output_timeout = "10m" }}


let Step =
  < SlackNotifyOnFail
      : { slack/notify : { event : Text, template : Text, branch_pattern : Text }}

  | SaveCache
      : { save_cache : { key : Text, paths : List Text }}

  | PersistToWorkspace
      : { persist_to_workspace : { root : Text, paths : List Text }}

  | RestoreCache
      : { restore_cache : { keys : List Text }}

  | StoreTestResults
      : { store_test_results : { path : Text }}

  | StoreArtifacts
      : { store_artifacts : { path : Text }}

  | AttachWorkspace
      : { attach_workspace : { at : Text }}

  | AddSSHKeys
      : { add_ssh_keys : { fingerprints : List Text }}

  | SetupRemoteDocker
      : { setup_remote_docker : { docker_layer_caching : Bool }}

  | Run
      : { run : Run.Type }

  | checkout
  | jq/install
  | shellcheck/install
  >


-- https://circleci.com/docs/2.0/configuration-reference/#docker-executor
let ResourceClass =
  <  small
  |  medium
  | `medium+`
  |  large
  |  xlarge
  | `2xlarge`
  | `2xlarge+`
  >


--------------------------------------------------------------------------------

let runT
   = \(no_output_timeout : Text)
  -> \(name              : Text)
  -> \(command           : Text)
  -> Step.Run { run = Run::{ name, command, no_output_timeout }}


let run
   = \(name    : Text)
  -> \(command : Text)
  -> Step.Run { run = Run::{ name, command }}


let restore_cache
   = \(keys : List Text)
  -> Step.RestoreCache { restore_cache = { keys }}


let save_cache
   = \(key   : Text)
  -> \(paths : List Text)
  -> Step.SaveCache { save_cache = { key, paths }}


let store_test_results
   = \(path : Text)
  -> Step.StoreTestResults { store_test_results = { path }}


let store_artifacts
   = \(path : Text)
  -> Step.StoreArtifacts { store_artifacts = { path }}


let persist_to_workspace
   = \(root  : Text)
  -> \(paths : List Text)
  -> Step.PersistToWorkspace { persist_to_workspace = { root, paths }}


let attach_workspace
   = \(at : Text)
  -> Step.AttachWorkspace { attach_workspace = { at }}


let add_ssh_keys
   = \(fingerprints : List Text)
  -> Step.AddSSHKeys { add_ssh_keys = { fingerprints }}


let setup_remote_docker
   = \(docker_layer_caching : Bool)
  -> Step.SetupRemoteDocker { setup_remote_docker = { docker_layer_caching }}


let mkdir_bin =
  run "mkdir -p ~/.local/bin"
      "mkdir -p ~/.local/bin"


let install_stack_deps = run "Install `stack` dependencies" ''
  sudo apt update && sudo apt install \
    g++ \
    gcc \
    git \
    gnupg \
    libc6-dev \
    libffi-dev \
    libgmp-dev \
    libtinfo-dev \
    make \
    netbase \
    xz-utils \
    zlib1g-dev
  ''


let slack/notify
  = Step.SlackNotifyOnFail { slack/notify =
    { event          = "fail"
    , template       = "basic_fail_1"
    , branch_pattern = "master,gh-pages"
    }}


--------------------------------------------------------------------------------

let DockerizedJob =
  { docker         : List { auth  : { password : Text, username : Text }
                          , image : Text
                          }
  , steps          : List Step
  , resource_class : ResourceClass
  }


let dockerized-job-with
   = \(image : Text)
  -> \(class : ResourceClass)
  -> \(steps : List Step)
  -> { docker         = [ default-docker-image // { image } ]
     , steps          = [ Step.checkout ] # steps
     , resource_class = class
     }


let dockerized-job
   = \(class : ResourceClass)
  -> \(steps : List Step)
  -> dockerized-job-with cimg-base class steps


let dockerized-job-with-build-core-bins
   = \(class : ResourceClass)
  -> \(steps : List Step)
  -> let s = [ attach_workspace "/tmp/build-core"
             , mkdir_bin
             , run "cp /tmp/build-core/bin/* ~/.local/bin"
                   "cp /tmp/build-core/bin/* ~/.local/bin"
             ] # steps
      in dockerized-job class s


let dockerized-job-with-build-core-bins-and-runner
   = \(class : ResourceClass)
  -> \(steps : List Step)
  -> let s = [ setup_remote_docker False
             , run "Attach runner image" ''
                 zcat /tmp/build-core/runner.tar.gz | docker load
                 ${docker-tag-all "runner"}
                 ''
             ] # steps
      in dockerized-job-with-build-core-bins class s


--------------------------------------------------------------------------------

let V_SOLC = "v0.8.2"
let V_Z3   = "4.8.10"
let S_Z3   = "x64-ubuntu-18.04"

let CACHE_DEPS_HS =
  "hs-3-{{ checksum \"hs/stack.yaml\" }}-{{ checksum \"hs/package.yaml\" }}"

let CACHE_DEPS_CI_REBUILD =
  "ci-rebuild-0-{{ checksum \".circleci/rebuild.hs\" }}"

let build-core = dockerized-job ResourceClass.medium
  [ mkdir_bin
  , run "Install `mo`" "curl -sSLo ~/.local/bin/mo https://git.io/get-mo"

  , run "Install `solc`" ''
      curl -sSLo ~/.local/bin/solc \
        https://github.com/ethereum/solidity/releases/download/${V_SOLC}/solc-static-linux
      ''

  , run "Install `z3`" ''
      curl -sSLo /tmp/z3.zip \
           https://github.com/Z3Prover/z3/releases/download/z3-${V_Z3}/z3-${V_Z3}-${S_Z3}.zip \
        && unzip -p /tmp/z3.zip z3-4.8.10-x64-ubuntu-18.04/bin/z3 \
         | cat > ~/.local/bin/z3
      ''

  , install_stack_deps
  , run "Install `stack`" ''
      curl -sSL https://get.haskellstack.org/ | sh -s - -d ~/.local/bin
      ''

  , run "chmod +x ~/.local/bin/*"
        "chmod +x ~/.local/bin/*"

  , run "Generate package.yaml" "cd hs && make package.yaml"

  -- https://github.com/reach-sh/reach-lang/blob/4742f3c/hs/Dockerfile.circleci#L79
  , run "Reset locale" ''
      sudo sh -c 'echo "LC_ALL=en_US.UTF-8" >> /etc/environment \
               && echo "en_US.UTF-8 UTF-8"  >> /etc/locale.gen \
               && echo "LANG=en_US.UTF-8"    > /etc/locale.conf \
               && locale-gen en_US.UTF-8'
    ''

  , restore_cache [ CACHE_DEPS_HS ]
  , run "Install hs dependencies"   "cd hs && make hs-deps"
  , save_cache      CACHE_DEPS_HS   [ "~/.stack", "hs/.stack-work" ]

  , run "Clean hs"         "cd hs && make hs-clean"
  , run "Build hs"         "cd hs && make hs-build"
  , run "Install `reachc`" "cd hs && stack install"

  , setup_remote_docker True

  , run "Build ethereum-devnet" "cd scripts/ethereum-devnet && make build"

  , run "Build js"                     "cd js && make build"
  , run "Check js/stdlib/package.json" "cd js/stdlib && make check"

  , run "Stash `build-core` workspace artifacts" ''
      mkdir -p /tmp/build-core/bin
      cp ~/.local/bin/* /tmp/build-core/bin
      docker save ${docker-image "runner" "latest"} | gzip > /tmp/build-core/runner.tar.gz
      ''
  , persist_to_workspace "/tmp/build-core" [ "runner.tar.gz", "bin" ]

  , run "Pull algorand-devnet" ''
      docker pull ${docker-image "algorand-devnet" VERSION_SHORT}
      docker run \
        --entrypoint /bin/sh \
        ${docker-image "algorand-devnet" VERSION_SHORT} \
        -c 'echo $REACH_GIT_HASH'
      ''

  -- TODO: make the build process deterministic,
  -- so that we can rebuild it on CI
  , run "Pull devnet-cfx" ''
      docker pull ${docker-image "devnet-cfx" VERSION_SHORT}
      docker run \
        --entrypoint /bin/sh \
        ${docker-image "devnet-cfx" VERSION_SHORT} \
        -c 'echo $REACH_GIT_HASH'
      ''

  , run "Is dockerhub up to date?" "scripts/docker-check.sh || echo 'XXX allowed to fail'"
  , slack/notify
  ]


let test-hs = dockerized-job-with-build-core-bins ResourceClass.medium
  [ install_stack_deps
  , run "Generate package.yaml" "cd hs && make package.yaml"
  , restore_cache [ CACHE_DEPS_HS ]

  , runT "20m"         "Test hs (xml)"   "cd hs && make hs-test-xml"
  , store_test_results "hs/test-reports"

  , run  "Check hs"    "cd hs && make hs-check"
  , store_artifacts    "hs/stan.html"

  , slack/notify
  ]

let dhallcheck = dockerized-job-with-build-core-bins ResourceClass.medium
  [ install_stack_deps

  , restore_cache [ CACHE_DEPS_CI_REBUILD ]
  , run "Run dhallcheck" "cd .circleci && make check"
  , save_cache      CACHE_DEPS_CI_REBUILD [ "~/.stack" ]

  , slack/notify
  ]


let test-js = dockerized-job-with-build-core-bins-and-runner ResourceClass.small
  [ restore_cache [ "hs-{{ .Revision }}" ]

  , run "Test js" "cd js/dockerized-tests && make clean-test && sbin/test.sh"

  , slack/notify
  ]


let docs-render = dockerized-job ResourceClass.small
  [ run "Install dependencies" ''
      sudo add-apt-repository -y ppa:plt/racket \
        && sudo apt update \
        && sudo apt install -y --no-install-recommends \
          libcairo2 \
          libfontconfig1 \
          libjpeg62 \
          libpangocairo-1.0 \
          racket \
          python3-setuptools
      ''

  , run "Install `pygments-reach`" "cd pygments && sudo make install"
  , run "Render docs"              "cd docs-src && make render"
  , store_artifacts "docs/"

  , run "Copy docs to workspace" ''
      mkdir -p /tmp/docs_workspace
      cp -r docs /tmp/docs_workspace/
      ''

  , persist_to_workspace "/tmp/docs_workspace" [ "docs" ]

  , slack/notify
  ]


let docs-deploy = dockerized-job-with "circleci/node:9.9.0" ResourceClass.small
  [ attach_workspace "/tmp/docs_workspace"

  -- gh-pages@3.0.0, not 3.1.0, because:
  -- https://github.com/tschaub/gh-pages/issues/354#issuecomment-647801438
  , run "Install and configure dependencies" ''
      # sudo npm install -g --silent gh-pages@3.0.0
      # git config user.email "ci-build@reach.sh"
      # git config user.name "ci-build"
      git config user.email "devbot@reach.sh"
      git config user.name "reachdevbot"
      ''

  , add_ssh_keys
      [ -- github:reachdevbot key - CircleCI: reach-sh/reach-lang
        "83:23:9c:21:6a:74:61:48:20:da:a3:45:79:89:3e:86"
        -- "b5:65:6d:49:4f:fb:c1:77:2c:13:45:b1:d9:f9:71:c6"
      ]

  , run "Deploy docs to gh-pages branch" ''
      git fetch origin gh-pages
      git checkout gh-pages
      git pull origin gh-pages
      git rm -r .
      cp -r /tmp/docs_workspace/docs/* .
      git add .
      # https://stackoverflow.com/questions/8123674/how-to-git-commit-nothing-without-an-error
      git diff-index --quiet HEAD || git commit -m "[ci skip] docs for $CIRCLE_SHA1"
      git push origin gh-pages
      ''

  , slack/notify
  ]


let shellcheck = dockerized-job ResourceClass.small
  [ Step.shellcheck/install
  , run "Run shellcheck" "make sh-lint"
  , slack/notify
  ]

let docker-lint = dockerized-job-with
  "hadolint/hadolint:v1.18.0-6-ga0d655d-alpine"
  ResourceClass.small
  [ run "Install dependencies" "apk add make bash curl jq"
  , run "Run hadolint"         "make docker-lint"
  , slack/notify
  ]


--------------------------------------------------------------------------------

let mk-example-run
   = \(directory : Text)
  -> \(connector : Connector)
  -> let c = Connector/show connector
      in runT "10m" "Run ${directory} with ${c}" ''
         cd examples && \
           REACH_CONNECTOR_MODE=${c} REACH_DEBUG=1 ./one.sh run ${directory}
      ''

let mk-example-job
   = \(nightly : Bool)
  -> \(ex      : KeyVal Text (List Connector))
  -> let key = if nightly
      then "${ex.mapKey}-nightly"
      else ex.mapKey

     let is-eth = \(c : Connector) -> merge
      { ETH  = True
      , ALGO = False
      , CFX  = False
      } c

     -- 2021-06-02: The `immediate` workflow should only target `ETH` for now;
     -- likewise, `nightly` is supposed to ignore the skip lists in
     -- `rebuild.hs` and try everything
     let conns = if not nightly
      then filter Connector is-eth ex.mapValue
      else [ Connector.ALGO
           , Connector.CFX
           , Connector.ETH
           ]

     let j = dockerized-job-with-build-core-bins-and-runner ResourceClass.small (
      [ run "Clean ${ex.mapKey}"   "cd examples && ./one.sh clean ${ex.mapKey}"
      , run "Rebuild ${ex.mapKey}" "cd examples && ./one.sh build ${ex.mapKey}"
      ]
      # map Connector Step (mk-example-run ex.mapKey) conns
      # [ slack/notify ])

     in `:=` DockerizedJob key j

let jobs =
  let `=:=` = `:=` DockerizedJob
   in [ `=:=` "build-core"  build-core
      , `=:=` "docs-render" docs-render
      , `=:=` "docs-deploy" docs-deploy
      , `=:=` "shellcheck"  shellcheck
      , `=:=` "docker-lint" docker-lint
      , `=:=` "test-hs"     test-hs
      , `=:=` "test-js"     test-js
      , `=:=` "dhallcheck"  dhallcheck
      ]
      -- `immediate`-workflow (ETH-only)
      # map (KeyVal Text (List Connector))
            (KeyVal Text DockerizedJob)
            (mk-example-job False)
            examples
      -- `nightly`-workflow
      # map (KeyVal Text (List Connector))
            (KeyVal Text DockerizedJob)
            (mk-example-job True)
            examples


--------------------------------------------------------------------------------

let workflows =
  let T =
    { Type    = { context  : List Text
                , requires : Optional (List Text)
                , filters  : Optional { branches : { only : Text }}
                }
    , default = { context  = [ "reachdevbot-on-dockerhub", "circleci-on-slack" ]
                , requires = None (List Text)
                , filters  = None { branches : { only : Text }}
                }
    }

  let `=:=` = `:=` T.Type

  let wf-docs-deploy =
    T::{ requires = Some [ "docs-render" ]
       , filters  = Some { branches = { only = "master" }}
       }

  let requires-build-core =
    T::{ requires = Some [ "build-core" ] }

  let mk-example-wf
     = \(nightly : Bool)
    -> \(ex      : KeyVal Text (List Connector))
    -> let key = if nightly
          then "${ex.mapKey}-nightly"
          else ex.mapKey
        in [ `=:=` key requires-build-core ]

  let lint = { jobs =
    [ [ `=:=` "shellcheck" T.default ]
    ] }

  let docs = { jobs =
    [ [ `=:=` "docs-render" T.default      ]
    , [ `=:=` "docs-deploy" wf-docs-deploy ]
    ] }

  let mk-build-and-test = \(nightly : Bool) ->
    let jobs =
      [ [ `=:=` "build-core" T.default           ]
      , [ `=:=` "test-hs"    requires-build-core ]
      , [ `=:=` "test-js"    requires-build-core ]
      , [ `=:=` "dhallcheck" requires-build-core ]
      ] # map (KeyVal Text (List Connector))
              (Map Text T.Type)
              (mk-example-wf nightly)
              examples

    let Schedule =
      { Type = { schedule :
        { cron    : Text
        , filters : { branches : { only : List Text }}
        }}
      , default = { schedule =
        { cron    = "0 0 * * *" -- 8pm EDT / Midnight UTC
        , filters = { branches = { only = [ "master" ] }}
        }}
      }

    let triggers = if nightly
      then Some [ Schedule.default ]
      else None (List Schedule.Type)

    in { jobs, triggers }

  let nightly   = mk-build-and-test True
  let immediate = mk-build-and-test False

  in { lint, docs, nightly, immediate }


--------------------------------------------------------------------------------

let orbs =
  { slack      = "circleci/slack@4.3.3"
  , shellcheck = "circleci/shellcheck@2.2.0"
  , jq         = "circleci/jq@2.2.0"
  }

in { version = 2.1, jobs , workflows, orbs }
