let version = 2.1


let orbs =
  { slack      = "circleci/slack@4.1.1"
  , shellcheck = "circleci/shellcheck@2.2.0"
  , jq         = "circleci/jq@2.2.0"
  }


let docker-creds =
  { auth = { username = "$DOCKER_LOGIN"
           , password = "$DOCKER_PASSWORD"
           }}


let docker-image =
    { image = "reachsh/reach-circle:0.1.2" }
  ∧ docker-creds


let default-job-setup =
  { context = [ "reachdevbot-on-dockerhub", "circleci-on-slack" ]}


{------------------------------------------------------------------------------}

let Run =
  { Type    = { name : Text, command : Text, no_output_timeout : Text }
  , default = { no_output_timeout = "10m" }
  }


let Step =
  < SlackNotifyOnFail  : { slack/notify         : { event        : Text, template : Text }}
  | SaveCache          : { save_cache           : { key          : Text, paths    : List Text }}
  | PersistToWorkspace : { persist_to_workspace : { root         : Text, paths    : List Text }}
  | RestoreCache       : { restore_cache        : { keys         : List Text }}
  | StoreTestResults   : { store_test_results   : { path         : Text }}
  | StoreArtifacts     : { store_artifacts      : { path         : Text }}
  | AttachWorkspace    : { attach_workspace     : { at           : Text }}
  | AddSSHKeys         : { add_ssh_keys         : { fingerprints : List Text }}
  | Run                : { run                  : Run.Type }
  | checkout
  | setup_remote_docker
  | jq/install
  >


{------------------------------------------------------------------------------}

let runWithin
  = λ(no_output_timeout : Text)
  → λ(name              : Text)
  → λ(command           : Text)
  → Step.Run { run = Run::{ name, command, no_output_timeout }}


let run
  = λ(name    : Text)
  → λ(command : Text)
  → Step.Run { run = Run::{ name, command }}


let restore_cache
  = λ(keys : List Text)
  → Step.RestoreCache { restore_cache = { keys }}


let save_cache
  = λ(key   : Text)
  → λ(paths : List Text)
  → Step.SaveCache { save_cache = { key, paths }}


let store_test_results
  = λ(path : Text)
  → Step.StoreTestResults { store_test_results = { path }}


let store_artifacts
  = λ(path : Text)
  → Step.StoreArtifacts { store_artifacts = { path }}


let persist_to_workspace
  = λ(root  : Text)
  → λ(paths : List Text)
  → Step.PersistToWorkspace { persist_to_workspace = { root, paths }}


let attach_workspace
  = λ(at : Text)
  → Step.AttachWorkspace { attach_workspace = { at }}


let add_ssh_keys
  = λ(fingerprints : List Text)
  → Step.AddSSHKeys { add_ssh_keys = { fingerprints }}


let slack/notify
  = Step.SlackNotifyOnFail { slack/notify = { event    = "fail"
                                            , template = "basic_fail_1"
                                            }}


{------------------------------------------------------------------------------}

let dockerized-job-with
  = λ(image : Text)
  → λ(steps : List Step)
  → { docker = docker-image // { image }
    , steps
    }


let dockerized-job
  = λ(steps : List Step)
  → { docker = docker-image
    , steps
    }


{------------------------------------------------------------------------------}

let build-and-test = dockerized-job
  [ Step.checkout

  , run "install mo" ''
      curl -sSL https://git.io/get-mo -o mo
        && chmod +x mo
        && mv mo /usr/local/bin
      ''

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
      ]

  , run "clean hs"      "cd hs && make hs-clean"
  , run "build hs"      "cd hs && make hs-build"
  , run "test hs (xml)" "cd hs && make hs-test-xml"
  , store_test_results  "hs/test-reports"

  , run "check hs"      "cd hs && make hs-check"
  , store_artifacts     "hs/stan.html"

  , Step.setup_remote_docker
  , run "build ethereum-devnet" "cd scripts/ethereum-devnet && make build"

  , run "build and test js"     "cd js && make build test"

  , run "rebuild examples"      "cd examples && make clean-all build-all"

  , run "pull algorand-devnet" ''
      docker pull reachsh/algorand-devnet:0.1
      docker run --entrypoint /bin/sh reachsh/algorand-devnet:0.1 -c 'echo $REACH_GIT_HASH'
      ''

  , runWithin "3m" "run examples" "cd examples && make run-all"

  , Step.jq/install

  , run "Is dockerhub up to date?" "scripts/docker-check.sh || echo 'XXX allowed to fail'"

  , slack/notify
  ]


let docs-render = dockerized-job
  [ Step.checkout

  , run "install pygments-reach" "cd pygments && make install"
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
  [ Step.checkout
  , attach_workspace "/tmp/docs_workspace"

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
  [ Step.checkout
  , run "Run shellcheck" "make sh-lint"
  , slack/notify
  ]


let docker-lint = dockerized-job-with "hadolint/hadolint:v1.18.0-6-ga0d655d-alpine"
  [ Step.checkout

  , run "install make, bash, curl, and jq" "apk add make bash curl jq"
  , run "run hadolint"                     "make docker-lint"

  , slack/notify
  ]


{------------------------------------------------------------------------------}

let jobs =
  { build-and-test
  , docs-render
  , docs-deploy
  , shellcheck
  , docker-lint
  }


let workflows =
  { build-and-test = { jobs = { build-and-test = default-job-setup }}
  , lint           = { jobs = { shellcheck     = default-job-setup }}
  , docs           = { jobs = { docs-render    = default-job-setup
                              , docs-deploy    = default-job-setup
                                  ∧ { requires = [ "docs-render" ]
                                    , filters  = { branches = { only = "master" }}}}}
  }


in
  { version
  , orbs
  , jobs
  , workflows
  }
