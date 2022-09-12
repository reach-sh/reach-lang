#!/usr/bin/env python3
import os, sys, time, shlex, subprocess, multiprocessing, examples

def cmd(command, redirect=None, timeout=None):
  print(">", command)
  try:
    return subprocess.call(shlex.split(command), stdout=redirect,
                           stderr=redirect, timeout=timeout)
  except subprocess.TimeoutExpired:
    return 'timeout'

def run_example(name):
  print("Running", name)
  artifact_name = f"{connector}.{name}"
  artifact_path = f"/tmp/artifacts/{artifact_name}"
  start_time = time.time()

  with open(artifact_path, "w") as artifact:
    cmd(f'./one.sh clean "{name}"', redirect=artifact)
    build_ret = cmd(f'./one.sh build "{name}"', redirect=artifact)
    if build_ret == 0:
      run_ret = cmd(f'./one.sh run "{name}"', redirect=artifact, timeout=600)
      if run_ret == 0:
        status = "pass"
        outcome = f"{name} passed"
      elif run_ret == 'timeout':
        status = "fail-time"
        outcome = f"{name} timed out"
      else:
        status = "fail"
        outcome = f"{name} failed"
    else:
      status = "fail"
      outcome = f"{name} failed to build"

  duration = int(time.time() - start_time)
  cmd(f'gzip "{artifact_path}"')
  print(outcome)

  with open(f"/tmp/test_results/{name}.xml", "w") as xml:
    maybe_fail = "" if status == "pass" else f'<failure message="{status}">{outcome}</failure>'
    xml.write(f"""
      <?xml version="1.0" encoding="UTF-8"?>
      <testsuite name="examples.{connector}">
        <testcase name="{name}" time="{duration}">
        {maybe_fail}
        </testcase>
      </testsuite>
    """)

  rank = os.environ["CIRCLE_NODE_INDEX"]
  with open(f'/tmp/workspace/record/{artifact_name}', "w") as record:
    record.write(f'["{status}", "{connector}.{rank}"]')

connector = sys.argv[1]
os.environ["REACH_DEBUG"] = "1"
os.environ["REACH_CONNECTOR_MODE"] = connector

os.chdir("../examples")
cmd("mkdir -p /tmp/artifacts /tmp/test_results /tmp/workspace/record")
cmd("../reach devnet --await-background")

examples_to_run = examples.circleci_split_examples(connector)
with multiprocessing.Pool(len(examples_to_run)) as pool:
  pool.map(run_example, examples_to_run)
