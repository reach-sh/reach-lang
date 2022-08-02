import json
import datetime
from statistics import mean, median, stdev
import go

def group_by_id(rows):
  groups = {}
  for row in rows:
    id = go.get_id(row)
    group = groups.get(id, [])
    groups[id] = group
    group.append(row)
  return groups

def d(s, unit):
  if unit == 'day':
    return datetime.datetime.strptime(s, "%Y-%m-%d")
  if unit == 'month':
    return datetime.datetime.strptime(f"{s}-01", '%Y-%m-%d')
  if unit == 'year':
    return datetime.datetime.strptime(f"{s}-01-01", '%Y-%m-%d')
  raise NotImplementedError

def dsub(x, y, unit):
  dx = d(x, unit)
  dy = d(y, unit)
  if unit == 'day':
    return (dx - dy).days
  if unit == 'month':
    return (dx.year * 12 + dx.month) - (dy.year * 12 + dy.month)
  if unit == 'year':
    return dx.year - dy.year
  raise NotImplementedError

def calc(groups, unit):
  results = []
  for id in groups:
    group = groups[id]
    ks = set(go.get_k(row, unit) for row in group)
    if '' in ks:
      ks.remove('')
    earliest = min(ks)
    latest = max(ks)
    length = dsub(latest, earliest, unit)
    results.append({
      'id': id,
      'count': len(group),
      'count_unique': len(ks),
      'earliest': earliest,
      'latest': latest,
      'length': length,
    })
  return results

def render(results):
  columns = ['id', 'count', 'count_unique', 'earliest', 'latest', 'length']
  yield ','.join(columns)
  for result in results:
    yield ','.join(str(result[col]) for col in columns)

def ab4(unit):
  if unit == 'day':
    # active now - 30 days or later
    d = go.now() - datetime.timedelta(days=30)
    return f"{d.year}-{go.zpad(d.month)}-{go.zpad(d.day)}"
  if unit == 'month':
    # active last month or later
    d = go.now()
    ysub = d.month == 1
    return f"{d.year-1 if ysub else d.year}-{go.zpad(12 if ysub else d.month-1)}"
  if unit == 'year':
    # active this year
    d = go.now()
    return f"{d.year}"
  raise NotImplemented

def summarize(results, unit):
  active_by = ab4(unit)
  lens = [r['length'] for r in results]
  actives = [r for r in results if r['latest'] >= active_by]
  oldest_active = min(actives, key=lambda r: r['earliest'])
  return {
    'user_count': len(lens),
    'min_earliest': min(r['earliest'] for r in results),
    'max_latest': max(r['latest'] for r in results),
    'min_length': min(lens),
    'median_length': median(lens),
    'mean_length': mean(lens),
    'max_length': max(lens),
    'stdev_length': stdev(lens),
    'active_cutoff': active_by,
    'active_count': len(actives),
    'oldest_active_id': oldest_active['id'],
    'oldest_active_length': oldest_active['length'],
  }

def stay(data, unit):
  rows = data['Items']
  groups = group_by_id(rows)
  results = calc(groups, unit)
  return results

def main():
  main_start = go.now()
  data = json.load(open('./data.json'))
  print('loading data took', go.now() - main_start)
  for unit in ['day', 'month', 'year']:
    fname = f"out/stay_{unit}.csv"
    print(f"writing to {fname}...")
    start = go.now()
    results = stay(data, unit)
    with open(fname, 'w') as f:
      for out in render(results):
        f.write(out)
        f.write('\n')
    print('  that took', go.now() - start)
    sfname = f"out/stay_{unit}_summary.json"
    print(f"writing summary to {sfname}")
    sstart = go.now()
    with open(sfname, 'w') as f:
      json.dump(summarize(results, unit), f)
    print('  that took', go.now() - sstart)
  print('done in', go.now() - main_start)

if __name__ == '__main__':
  main()
