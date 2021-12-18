import fetch from 'node-fetch';
import waitPort from 'wait-port';

const hostname = "http://localhost"
const port = 3001
const address = `${hostname}:${port}`

const waitForPort = async () => {
  const params = {
    port: port
  }
  const r = await waitPort(params)
  console.log(r)
}

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

const initFor = async (s,a) => {
  const r = await interact('POST', `${address}/init/${a}/${s}`, {})
  console.log(r)
  return r
}

const changeActor = async (s) => {
  const r = await interact('POST', `${address}/change_actor/${s}`, {})
  console.log(r)
  return r
}

const ping = async () => {
  const r = await fetch(`${address}/ping`)
  console.log(r)
  return r.json();
}

export {
  getStates,
  getStatus,
  getStateActions,
  load,
  init,
  respondWithVal,
  ping,
  waitForPort,
  changeActor,
  initFor
};
