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
  entry = lambda s: (len(s) > 1) and (not s.startswith("#"))
  entries = filter(entry, lines)
  parse = lambda s: s.strip().split(" ")
  parsed = map(parse, entries)
  for_connector = lambda l: any(c == connector or c == "all" for c in l[1:])
  for_connector = filter(for_connector, parsed)
  example_names = map(lambda l: l[0], for_connector)
  return list(set(example_names))

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
  answer = n_special + math.ceil(n_regular / EXAMPLES_DIVISOR)
  return answer

# Divide list into n approximately equal chunks
def divide_list(lst, n):
  k, m = divmod(len(lst), n)
  return list(lst[i*k + min(i, m): (i+1)*k + min(i+1, m)] for i in range(n))

# Divides examples for a connector such that each special example is given its own
# solo runner and regular examples are divided evenly among remaining runners.
# Returns a list of examples for the current runner to execute.
def get_examples_to_run(connector):
  special_examples = get_special_for(connector)
  regular_examples = get_regular_for(connector)
  parallelism = parallel_jobs_for(connector)
  assert parallelism == int(os.environ["CIRCLE_NODE_TOTAL"])
  num_regular_jobs = parallelism - len(special_examples)
  regular_jobs = divide_list(regular_examples, num_regular_jobs)
  special_jobs = divide_list(special_examples, len(special_examples))
  jobs = special_jobs + regular_jobs
  i = int(os.environ["CIRCLE_NODE_INDEX"])
  return jobs[i]

