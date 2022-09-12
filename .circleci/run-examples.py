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
  artifact_path = f"/tmp/artifacts/{name}"
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
  with open(f'/tmp/workspace/record/{name}', "w") as record:
    record.write(f'["{status}", "{connector}.{rank}"]')

def run_multiple_examples(examples):
  multiprocessing.Pool(len(examples)).map(run_example, examples)

connector = sys.argv[1]
os.environ["REACH_DEBUG"] = "1"
os.environ["REACH_CONNECTOR_MODE"] = connector

os.chdir("../examples")
cmd("mkdir -p /tmp/artifacts /tmp/test_results /tmp/workspace/record")
cmd("../reach devnet --await-background")

# Generate what to pass to stdin for `circleci tests split`
# The split command takes some lines, then magically splits the lines
# between runners and prints which lines the current runner is assigned.
# We pass it exactly the same number of lines as there are parallel runners,
# so we get exactly one line back per runner.
#
# This passes every special example on its own line, and
# for every group of regular examples, passes "job-group-<n>".
(special_examples, regular_example_lists) = examples.split_examples_into_jobs(connector)
split_args = special_examples + [f"job-group-{n}" for n in range(len(regular_example_lists))]
assert len(split_args) == int(os.environ["CIRCLE_NODE_TOTAL"])

split_args_bytes = "\n".join(split_args).encode('utf-8')
circleci_split = subprocess.run(["circleci", "tests", "split"], input=split_args_bytes,
                                 stdout=subprocess.PIPE, check=True)
job = circleci_split.stdout.decode('utf-8')
assert job.count("\n") == 1
job = job.strip()

if job.startswith("job-group-"):
  # Running a group of regular examples
  index = int(job.rsplit("-", maxsplit=1)[-1])
  examples = regular_example_lists[index]
  run_multiple_examples(examples)
else:
  # Running a special example
  run_example(job)
