let Prelude =
  https://prelude.dhall-lang.org/v20.1.0/package.dhall
  sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let Map = Prelude.Map.Type
let map = Prelude.List.map

let VERSION = env:VERSION as Text

-- TODO use `VERSION` instead of hard-coded "0.1" once corresponding XXX
--   RE: `REACH_DEFAULT_VERSION` in `reach` script has been completed
let runner-image = "reachsh/runner:0.1"


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

let reach-circle =
  "reachsh/reach-circle:${VERSION}"


let docker-creds =
  { auth = { username = "$DOCKER_LOGIN"
           , password = "$DOCKER_PASSWORD"
           }}


let default-docker-image =
    { image = reach-circle } /\ docker-creds


--------------------------------------------------------------------------------

let Run =
  { default = { no_output_timeout = "10m" }
  , Type    = { name              : Text
              , command           : Text
              , no_output_timeout : Text
              }
  }


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


let install_mo =
  run "install mo" ''
      curl -sSL https://git.io/get-mo -o mo \
        && chmod +x mo \
        && mv mo /usr/local/bin
      ''


let slack/notify
  = Step.SlackNotifyOnFail { slack/notify =
    { event          = "fail"
    , template       = "basic_fail_1"
    , branch_pattern = "master,gh-pages"
    }}


--------------------------------------------------------------------------------

let DockerizedJob =
  { docker : List { auth  : { password : Text, username : Text }
                  , image : Text
                  }
  , steps  : List Step
  }


let dockerized-job-with
   = \(image : Text)
  -> \(steps : List Step)
  -> { docker = [ default-docker-image // { image } ]
     , steps  = [ Step.checkout ] # steps
     }


let dockerized-job-with-reach-circle
   = \(steps : List Step)
  -> dockerized-job-with reach-circle steps


let dockerized-job-with-reach-circle-and-runner
   = \(steps : List Step)
  -> let s = [ attach_workspace    "/tmp/build-core"
             , setup_remote_docker False -- TODO toggle caching on: True
             , run "attach runner image" ''
                 zcat /tmp/build-core/runner.tar.gz | docker load
                 ''
             ] # steps
      in dockerized-job-with-reach-circle s


--------------------------------------------------------------------------------

let build-core = dockerized-job-with-reach-circle
  [ install_mo

  , run "hs package.yaml" "cd hs && make package.yaml"
  , restore_cache
      [ "hs-2-{{ checksum \"hs/stack.yaml\" }}-{{ checksum \"hs/package.yaml\" }}"
      , "hs-2-{{ checksum \"hs/stack.yaml\" }}"
      , "hs-2-"
      , "hs-"
      ]

  , run "install hs dependencies" "cd hs && make hs-deps"
  , save_cache
      "hs-2-{{ checksum \"hs/stack.yaml\" }}-{{ checksum \"hs/package.yaml\" }}"
      [ "/root/.stack"
      , "hs/.stack_work"
      , "hs/.stack-work"
      ]

  , run "clean hs" "cd hs && make hs-clean"
  , run "build hs" "cd hs && make hs-build"
  , save_cache
      "hs-{{ .Revision }}"
      [ "/root/.stack"
      , "hs/.stack-work"
      ]

  , setup_remote_docker False -- TODO toggle caching on: True

  , run "build ethereum-devnet" "cd scripts/ethereum-devnet && make build"
  , run "build js"              "cd js && make build"

  , run "stash runner image" ''
      mkdir -p /tmp/build-core
      docker save ${runner-image} | gzip > /tmp/build-core/runner.tar.gz
      ''
  -- TODO alleviate cache time penalty by putting `reachc` in here too?
  , persist_to_workspace "/tmp/build-core" [ "runner.tar.gz" ]

  , run "pull algorand-devnet" ''
      docker pull reachsh/algorand-devnet:0.1
      docker run --entrypoint /bin/sh reachsh/algorand-devnet:0.1 -c 'echo $REACH_GIT_HASH'
      ''

  , Step.jq/install
  , run "Is dockerhub up to date?" "scripts/docker-check.sh || echo 'XXX allowed to fail'"
  , slack/notify
  ]


let test-hs = dockerized-job-with-reach-circle
  [ restore_cache [ "hs-{{ .Revision }}" ]

  , runT "20m"         "test hs (xml)"   "cd hs && make hs-test-xml"
  , store_test_results "hs/test-reports"

  , run  "check hs"    "cd hs && make hs-check"
  , store_artifacts    "hs/stan.html"

  , Step.jq/install
  , slack/notify
  ]


let test-js = dockerized-job-with-reach-circle-and-runner
  [ run "test js" "cd js && make test"
  , Step.jq/install
  , slack/notify
  ]


let docs-render = dockerized-job-with-reach-circle
  [ run "install pygments-reach" "cd pygments && make install"
  , run "render docs"            "cd docs-src && make render"
  , store_artifacts "docs/"

  , run "copy docs to workspace" ''
      mkdir -p /tmp/docs_workspace
      cp -r docs /tmp/docs_workspace/
      ''

  , persist_to_workspace "/tmp/docs_workspace" [ "docs" ]

  , slack/notify
  ]


let docs-deploy = dockerized-job-with "circleci/node:9.9.0"
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


let shellcheck = dockerized-job-with "cimg/base:stable"
  [ Step.shellcheck/install
  , run "Run shellcheck" "make sh-lint"
  , slack/notify
  ]


let docker-lint = dockerized-job-with "hadolint/hadolint:v1.18.0-6-ga0d655d-alpine"
  [ run "install make, bash, curl, and jq" "apk add make bash curl jq"
  , run "run hadolint"                     "make docker-lint"

  , slack/notify
  ]


--------------------------------------------------------------------------------

let mk-example-job
   = \(directory : Text)
  -> let j = dockerized-job-with-reach-circle-and-runner
      [ install_mo
      , restore_cache [ "hs-{{ .Revision }}" ]

      , run       "clean ${directory}"   "cd examples && ./one.sh clean ${directory}"
      , run       "rebuild ${directory}" "cd examples && ./one.sh build ${directory}"
      , runT "5m" "run ${directory}"     "cd examples && ./one.sh run ${directory}"

      , Step.jq/install
      , slack/notify
      ]
    in `:=` DockerizedJob directory j


let jobs =
  let `=:=` = `:=` DockerizedJob
   in [ `=:=` "build-core"  build-core
      , `=:=` "docs-render" docs-render
      , `=:=` "docs-deploy" docs-deploy
      , `=:=` "shellcheck"  shellcheck
      , `=:=` "docker-lint" docker-lint
      , `=:=` "test-hs"     test-hs
      , `=:=` "test-js"     test-js
      ] # map Text (KeyVal Text DockerizedJob) mk-example-job ./examples.dhall


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
     = \(directory : Text)
    -> [ `=:=` directory requires-build-core ]

  let lint =
    { jobs = [[ `=:=` "shellcheck" T.default ]] }

  let docs = { jobs =
    [ [ `=:=` "docs-render" T.default  ]
    , [ `=:=` "docs-deploy" wf-docs-deploy ]
    ]}

  let build-and-test = { jobs =
    [ [ `=:=` "build-core" T.default           ]
    , [ `=:=` "test-hs"    requires-build-core ]
    , [ `=:=` "test-js"    requires-build-core ]
    ] # map Text (Map Text T.Type) mk-example-wf ./examples.dhall
    }

  in { lint, docs, build-and-test }


--------------------------------------------------------------------------------

let orbs =
  { slack      = "circleci/slack@4.3.3"
  , shellcheck = "circleci/shellcheck@2.2.0"
  , jq         = "circleci/jq@2.2.0"
  }

in { version = 2.1, jobs , workflows, orbs }
