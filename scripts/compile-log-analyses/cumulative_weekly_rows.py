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
  count = 0
  yws = {}
  for item in items:
     ic = getic(item)
     if not ic:
       continue
     ws = yws.get(ic.year, {})
     yws[ic.year] = ws
     w = ws.get(ic.week, 0) + 1
     ws[ic.week] = w
  return yws


def display_counts(ywcounts):
  tot = 0
  for yk in sorted(ywcounts.keys()):
    wcounts = ywcounts[yk]
    for wk in sorted(wcounts.keys()):
      count = wcounts[wk]
      tot += count
      wkstr = f'0{wk}' if wk < 10 else wk
      print(f'{yk}, week {wkstr}: {tot} (+{count})')


def main():
  data = json.load(open('./data.json'))
  items = data['Items']
  yws = calc_yws(items)
  display_counts(yws)


if __name__ == '__main__':
  main()
