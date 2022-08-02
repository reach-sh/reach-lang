import json
import datetime
import sys
import os

units = ['year', 'month', 'day', 'week']

def fail_usage():
  print(f"usage: python3 go.py [all,{','.join(units)}]", file=sys.stderr)
  sys.exit(1)

def get_unit():
  if (len(sys.argv) <= 1):
    return 'all'
  unit = sys.argv[1]
  if (unit not in units and unit != 'all'):
    fail_usage()
  return unit

def get_id(row: any) -> str:
  return row['userId']['S']

def zpad(n: int) -> str:
  if (n <= 9):
    return f"0{n}"
  else:
    return f"{n}"

def get_k(row: any, unit: str) -> str:
  s = row['startTime']['S']
  try:
    d = datetime.datetime.fromisoformat(s[0:10])
    i = d.isocalendar()
  except:
    return ''
  if unit == 'year':
    return f"{d.year}"
  if unit == 'month':
    return f"{d.year}-{zpad(d.month)}"
  if unit == 'day':
    return f"{d.year}-{zpad(d.month)}-{zpad(d.day)}"
  if unit == 'week':
    return f"{i.year}-week{zpad(i.week)}"
  raise RuntimeError(f"Unexpected unit: {unit}")

def group_per_unit(rows: list, unit: str) -> dict:
  groups = {}
  for row in rows:
     k = get_k(row, unit)
     group = groups.get(k, [])
     groups[k] = group
     group.append(row)
  groups.pop('', None)
  return groups

def calc(groups: dict) -> list:
  results = []
  all_ids = set()
  all_rows_thru = 0
  for period in sorted(groups.keys()):
    group = groups[period]
    rows = len(group)
    all_rows_thru += rows
    ids = set(get_id(row) for row in group)
    new_ids = ids.difference(all_ids)
    all_ids.update(new_ids)
    results.append({
      'period': period,
      'row_count': rows,
      'cumulative_row_count': all_rows_thru,
      'unique_users': len(ids),
      'new_users': len(new_ids),
      'cumulative_new_users': len(all_ids),
    })
  return results

def render(results: list):
  columns = ['period', 'row_count', 'cumulative_row_count', 'unique_users', 'new_users', 'cumulative_new_users']
  yield ','.join(columns)
  for result in results:
    yield ','.join(str(result[col]) for col in columns)

def go(data: dict, unit: str):
  items = data['Items']
  groups = group_per_unit(items, unit)
  results = calc(groups)
  return render(results)

def now():
  return datetime.datetime.now()

def main():
  main_start = now()
  unit = get_unit()
  data = json.load(open('./data.json'))
  if (unit == 'all'):
    print('loading data took', now() - main_start)
    if not os.path.exists('out'):
      os.mkdir('out')
    for unit in units:
      start = now()
      fname = f"out/{unit}.csv"
      print(f"working on {fname}...")
      with open(fname, 'w') as f:
        for out in go(data, unit):
          f.write(out)
          f.write('\n')
      print('  that took', now() - start)
    print('done in', now() - main_start)
  else:
    for out in go(data, unit):
      print(out)

if __name__ == '__main__':
  main()
