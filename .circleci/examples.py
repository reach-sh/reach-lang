#!/usr/bin/env python3
import os, math
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
