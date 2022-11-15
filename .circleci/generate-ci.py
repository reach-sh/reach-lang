#!/usr/bin/env python3
import os, sys, examples
from pathlib import Path

if os.environ.get("CIRCLECI"):
  os.system("sudo apt-get install python3-ruamel.yaml")
from ruamel.yaml import YAML

yaml = YAML(typ="rt")
config = yaml.load(Path("config.pre.yml"))
build_sink_deps = []

# Insert an entry into config.gen.yml in the build workflow:
# workflows:
#   build:
#     jobs:
#       - <base_job>:
#           <prop_1_key>: <prop_1_val>
#           <prop_2_key>: <prop_2_val>
#         ...
def add_build_job(base_job, properties):
  properties["context"] = ["reachdevbot-aws-ecr"]
  config["workflows"]["build"]["jobs"].append({ base_job: properties })

def dependencies(deps):
  return {
    "deps": " ".join(deps),
    "requires": list(map(lambda name: f"build/{name}", deps))
  }

def build_image(executor, name, deps):
  add_build_job("build-image", {
    "name": f"build/{name}",
    "image": name,
    "exec": executor,
    **dependencies(deps)
  })

def hs_test(mode):
  name = f"hs-test-{mode}"
  add_build_job("hs-test", {
    "name": name,
    "mode": mode,
    **dependencies([f"haskell-build-artifacts-{mode}"])
  })
  build_sink_deps.append(name)

def connector(executor, connector):
  examples_name = f"examples.{connector}"
  browser_tests_name = f"browser-tests.{connector}"
  devnet_name = f"devnet-{connector.lower()}"

  build_image(executor, devnet_name, [])
  add_build_job("examples", {
    "name": examples_name,
    "connector": connector,
    "size": examples.parallel_jobs_for(connector),
    **dependencies([devnet_name, "reach", "reach-cli",
                    "runner", "react-runner", "rpc-server"])
  })
  add_build_job("browser-tests", {
    "name": browser_tests_name,
    "connector": connector,
    **dependencies([devnet_name, "reach", "reach-cli", "react-runner"])
  })
  build_sink_deps.append(examples_name)
  build_sink_deps.append(browser_tests_name)

build_image("real", "haskell-build-artifacts-open",   ["devnet-algo"])
build_image("real", "haskell-build-artifacts-closed", ["devnet-algo"])
build_image("fake", "reach",                          ["haskell-build-artifacts-open"])
build_image("fake", "reach-cli",                      ["haskell-build-artifacts-open"])
build_image("real", "js-deps",                        [])
build_image("real", "stdlib",                         ["reach", "js-deps"])
build_image("fake", "runner",                         ["stdlib"])
build_image("fake", "react-runner",                   ["stdlib", "js-deps"])
build_image("fake", "rpc-server",                     ["runner"])

hs_test("open")
hs_test("closed")

connector("fake", "ALGO")
connector("fake", "ETH")

add_build_job("build-sink", { "requires": build_sink_deps })

yaml.dump(config, Path("config.gen.yml"))
