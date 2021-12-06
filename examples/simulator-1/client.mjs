import fetch from 'node-fetch';
import assert from 'assert';
import waitPort from 'wait-port';

// const fetch = require('node-fetch')

const hostname = "host.docker.internal"
const port = 3001
const address = `${hostname}:${port}`

const getStates = async () => {
  const r = await fetch(`${address}/states`)
  console.log(r)
  return r.json();
}

const getStatus = async () => {
  const r = await fetch(`${address}/status`)
  console.log(r)
  return r.json();
}

async function getStateActions(s) {
  const r = await fetch(`${address}/states/${s}/actions`)
  console.log(r)
  return r.json();
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
  const r = await interact('POST', `${address}/load`, {})
  console.log(r)
  return r;
}

const init = async () => {
  const r = await interact('POST', `${address}/init`, {})
  console.log(r)
  return r
}

const respondWithVal = async (s,a,v,t) => {
  const r = await interact('POST', `${address}/states/${s}/actions/${a}/?data=${v}&type=${t}`, {})
  console.log(r)
  return r
}

const ping = async () => {
  const r = await fetch(`${address}/ping`)
  console.log(r)
  return r.json();
}

const main = async () => {

  const params = {
    port: port
  };

  await waitPort(params)
  await ping()
  await load()
  await init()
  await respondWithVal(0,0,0,"Number")
  await respondWithVal(1,1,"Alice","String")
  await respondWithVal(2,2,1,"Number")
  await respondWithVal(3,3,"Bob","String")
  await respondWithVal(4,4,0,"Number")
  await respondWithVal(5,5,0,"Number")
  const r = await getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
