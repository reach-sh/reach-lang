import json
import datetime

def getic(item):
  s = item['startTime']['S']
  try:
    d = datetime.datetime.fromisoformat(s[0:10])
    return d.isocalendar()
  except:
    print('SKIPPING:', s)
    return None


def getid(item):
  return item['userId']['S']


def calc_yws(items):
  yws = {}
  for item in items:
     ic = getic(item)
     if not ic:
       continue
     id = getid(item)
     ws = yws.get(ic.year, {})
     yws[ic.year] = ws
     w = ws.get(ic.week, set())
     ws[ic.week] = w
     w.add(id)
  return yws

def calc_cumulative(yws):
  ids = set()
  ywcounts = {}
  for yk in sorted(yws.keys()):
    ws = yws[yk]
    for wk in sorted(ws.keys()):
      wcounts = ywcounts.get(yk, {})
      ywcounts[yk] = wcounts
      ids = ids.union(ws[wk])
      wcounts[wk] = len(ids)
  return ywcounts


def display_counts(ywcounts):
  for yk in sorted(ywcounts.keys()):
    wcounts = ywcounts[yk]
    for wk in sorted(wcounts.keys()):
      count = wcounts[wk]
      wkstr = f'0{wk}' if wk < 10 else wk
      print(f'{yk}, week {wkstr}: {count}')


def calc_longs(yws):
  longs = {}
  # yws[yk][wk] == set(id)
  for yk in yws:
    for wk in yws[yk]:
      for id in yws[yk][wk]:
        count = longs.get(id, 0) + 1
        longs[id] = count
  return longs


def display_longs(longs):
  t = {}
  for lk in longs.keys():
    count = longs[lk]
    ts = t.get(count, set())
    t[count] = ts
    ts.add(lk)
  for tk in sorted(t.keys(), reverse=True):
    print(f'{tk} weeks: {len(t[tk])}')


def main():
  data = json.load(open('./data.json'))
  items = data['Items']
  yws = calc_yws(items)
  # ywcounts = calc_cumulative(yws)
  # display_counts(ywcounts)
  longs = calc_longs(yws)
  display_longs(longs)


if __name__ == '__main__':
  main()
