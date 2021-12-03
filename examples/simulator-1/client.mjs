import fetch from 'node-fetch';
import assert from 'assert';

// const fetch = require('node-fetch')

const hostname = "http://localhost:3000"

const getStates = async () => {
  const r = fetch(`${hostname}/states`)
    .then(response => response.json())
    .then(data => data);
  console.log(r)
  return r
}

const getStatus = async () => {
  const r = await fetch(`${hostname}/status`)
    .then(response => response.json())
    .then(data => data);
  console.log(r)
  return r
}

async function getStateActions(s) {
  const r = await fetch(`${hostname}/states/${s}/actions`)
    .then(response => response.json())
    .then(data => data);
  console.log(r)
  return r
}

async function interact(method = 'GET', url = '', data = {}) {
  const response = await fetch(url, {
    method: method,
    mode: 'cors',
    cache: 'no-cache',
    credentials: 'same-origin',
    headers: {
      'Content-Type': 'application/json'
    },
    redirect: 'follow',
    referrerPolicy: 'no-referrer',
    body: JSON.stringify(data)
  });
  return response.json();
}

const load = async () => {
  const r = await interact('POST', `${hostname}/load`, {})
    .then(data => data);
  console.log(r)
  return r
}

const init = async () => {
  const r = await interact('POST', `${hostname}/init`, {})
    .then(data => data);
  console.log(r)
  return r
}

const respondWithVal = async (s,a,v,t) => {
  const r = await interact('POST', `${hostname}/states/${s}/actions/${a}/?data=${v}&type=${t}`, {})
    .then(data => data);
  console.log(r)
  return r
}

const ping = async () => {
  const r = await fetch(`${hostname}/ping`)
    .then(response => response.json())
    .then(data => data);
  console.log(r)
  return r
}

const sleep = (delay) => new Promise((resolve) => setTimeout(resolve, delay))

const main = async () => {
  await sleep(10000)
  ping()
  await sleep(1000)
  load()
  await sleep(1000)
  init()
  await sleep(1000)
  respondWithVal(0,0,0,"Number")
  await sleep(1000)
  respondWithVal(1,1,"Alice","String")
  await sleep(1000)
  respondWithVal(2,2,1,"Number")
  await sleep(1000)
  respondWithVal(3,3,"Bob","String")
  await sleep(1000)
  respondWithVal(4,4,0,"Number")
  await sleep(1000)
  respondWithVal(5,5,0,"Number")
  const r = await getStatus()
  await sleep(1000)
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
