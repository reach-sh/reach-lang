import requests
import csv
import json
import sys

def getInfo(ip):
  url = "http://api.ipstack.com/" + ip + "?access_key=5abb1d1c4e614029ec30c94dee269d19"
  response = requests.request("GET", url, params={"format":"json"})
  return json.loads(response.text)

filename = sys.argv[1]

inputFile = open(filename, newline='')
reader = csv.DictReader(inputFile)

csvFile = open('location.csv', 'w')
csvFile.write('month,continent,country,region,latitude,longitude,compile_count_for_ip\n')

for row in reader:
  ip = row['ip_address']
  response = getInfo(ip)
  csvFile.writelines(','.join([
    row['month'],
    response['continent_name'],
    response['country_name'],
    response['region_name'],
    str(response['latitude']),
    str(response['longitude']),
    row['count']
  ]) + "\n")

csvFile.close()
inputFile.close()
