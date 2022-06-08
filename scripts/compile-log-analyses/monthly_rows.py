import json
import datetime


def getd(item):
  s = item['startTime']['S']
  try:
    d = datetime.datetime.fromisoformat(s[0:10])
    # return d.isocalendar()
    return d
  except:
    # print('SKIPPING:', s)
    return None


def getid(item):
  return item['userId']['S']


def calc_yms(items):
  count = 0
  yms = {}
  for item in items:
     ic = getd(item)
     if not ic:
       continue
     ws = yms.get(ic.year, {})
     yms[ic.year] = ws
     m = ws.get(ic.month, 0) + 1
     ws[ic.month] = m
  return yms


def display_counts(ymcounts):
  tot = 0
  print('year-month, count, total')
  for yk in sorted(ymcounts.keys()):
    mcounts = ymcounts[yk]
    for m in sorted(mcounts.keys()):
      count = mcounts[m]
      tot += count
      mstr = f'0{m}' if m < 10 else f'{m}'
      print(f'{yk}-{mstr}, {count}, {tot}')


def main():
  data = json.load(open('./data.json'))
  items = data['Items']
  yms = calc_yms(items)
  display_counts(yms)


if __name__ == '__main__':
  main()
