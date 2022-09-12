#!/usr/bin/env python3
import os, math, subprocess
from functools import lru_cache

EXAMPLES_DIVISOR = 16
circleci_dir = os.path.realpath(os.path.dirname(__file__))

# Return names of all examples in <repo>/examples
@lru_cache
def get_all():
  examples_dir = f"{circleci_dir}/../examples/"
  examples_ls = os.listdir(examples_dir)
  dirs_only = filter(lambda ex: os.path.isdir(f"{examples_dir}/{ex}"), examples_ls)
  return list(dirs_only)

# Return names of special examples that should not run with other examples / that are
# written in "special-examples.txt"
@lru_cache
def get_special_for(connector):
  with open(f"{circleci_dir}/special-examples.txt", "r") as f:
    lines = f.readlines()
  no_comments = filter(lambda s: (len(s) > 1) and (not s.startswith("#")), lines)
  split_lines = map(lambda s: s.strip().rsplit(".", maxsplit=1), no_comments)
  only_for_connector = filter(lambda l: connector == l[1], split_lines)
  example_names = map(lambda l: l[0], only_for_connector)
  return list(example_names)

# Return names of all examples that can be run in parallel / that are not
# written in "special-examples.txt"
@lru_cache
def get_regular_for(connector):
  all_examples = get_all()
  special_examples = get_special_for(connector)
  not_special = filter(lambda ex: ex not in special_examples, all_examples)
  return list(not_special)

# Computes the number to pass to CircleCI's 'parallelism' parameter for "examples.<connector>"
# See usage in generate-ci.py
def parallel_jobs_for(connector):
  n_special = len(get_special_for(connector))
  n_regular = len(get_regular_for(connector))
  return n_special + math.ceil(n_regular / EXAMPLES_DIVISOR)

# Divide list into n approximately equal chunks
def divide_list(lst, n):
  k, m = divmod(len(lst), n)
  return list(lst[i*k + min(i, m): (i+1)*k + min(i+1, m)] for i in range(n))

# For a given connector, computes how to share the examples between the CI parallel jobs.
# Each special example is should be given its own parallel job
# Regular examples should be spread evenly among the remaining parallel jobs
# Returns a 2-elem tuple, the first element is the list of special examples, the second is
#   a list of lists of regular jobs, divided into <parallel_jobs - special_jobs> chunks
#   (i.e. divided evenly into remaining parallel jobs)
def split_examples_into_jobs(connector):
  special_examples = get_special_for(connector)
  regular_examples = get_regular_for(connector)
  parallel_jobs = parallel_jobs_for(connector)
  regular_example_jobs = parallel_jobs - len(special_examples)
  return (special_examples, divide_list(regular_examples, regular_example_jobs))

# Call `circleci tests split` using information from split_examples_into_jobs.
# The split command takes some lines, then magically splits the lines evenly
# between runners and prints which lines the current runner is assigned.
# We pass it exactly the same number of lines as there are parallel runners,
# so we get exactly one line back per runner.
#
# This passes every special example on its own line, and
# for every group of regular examples, passes "job-group-<n>".
# Then, it decodes "job-group-*" lines back into a list of examples, 
# and returns a list of examples for the current runner to execute.
def circleci_split_examples(connector):
  (special_examples, regular_examples) = split_examples_into_jobs(connector)
  split_args = special_examples + [f"job-group-{n}" for n in range(len(regular_examples))]
  assert len(split_args) == int(os.environ["CIRCLE_NODE_TOTAL"])
  split_args_bytes = "\n".join(split_args).encode('utf-8')
  circleci_split = subprocess.run(["circleci", "tests", "split"], input=split_args_bytes,
                                   stdout=subprocess.PIPE, check=True)
  job = circleci_split.stdout.decode('utf-8')
  assert job.count("\n") == 1
  job = job.strip()

  if job.startswith("job-group-"):
    # Running a group of regular examples
    index = int(job.split("-")[-1])
    return regular_examples[index]
  else:
    return [job]

