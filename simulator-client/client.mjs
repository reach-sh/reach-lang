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

async function interact(method = 'GET', url = '', data = {}) {
  const response = await fetch(url, {
    method: method,
    cache: 'no-cache',
    headers: {
      'Content-Type': 'application/json'
    },
    redirect: 'follow',
    referrerPolicy: 'no-referrer'
  });
  return response.json();
}

const getStates = async () => {
  const r = await interact(`GET`, `${address}/states`)
  console.log(r)
  return r;
}

const getEdges = async () => {
  const r = await interact(`GET`, `${address}/edges`)
  console.log(r)
  return r;
}

const getStatus = async () => {
  const r = await interact(`GET`, `${address}/status`)
  console.log(r)
  return r;
}

async function getActions(s,a) {
  const r = await interact(`GET`, `${address}/actions/${s}/${a}`)
  console.log(r)
  return r;
}

async function getStateGlobals(s) {
  const r = await interact(`GET`, `${address}/global/${s}`)
  console.log(r)
  return r;
}

async function getStateLocals(s) {
  const r = await interact(`GET`, `${address}/local/${s}`)
  console.log(r)
  return r;
}

async function getLoc(s,a) {
  const r = await interact(`GET`, `${address}/locs/${s}/${a}`)
  console.log(r)
  return r;
}

const load = async () => {
  const r = await interact('POST', `${address}/load`)
  console.log(r)
  return r;
}

const init = async () => {
  const r = await interact('POST', `${address}/init`)
  console.log(r)
  return r
}

const respondWithVal = async (s,a,v,w=false,t='number') => {
  const who = (w || w === 0) ? `&who=${w}` : ``
  const r = await interact('POST', `${address}/states/${s}/actions/${a}/?data=${v}${who}&type=${t}`)
  console.log(r)
  return r
}

const initFor = async (s,a) => {
  const r = await interact('POST', `${address}/init/${a}/${s}`)
  console.log(r)
  return r
}

const newAccount = async (s) => {
  const r = await interact('POST', `${address}/accounts/new/${s}`)
  console.log(r)
  return r
}

const newToken = async (s) => {
  const r = await interact('POST', `${address}/tokens/new/${s}`)
  console.log(r)
  return r
}

const transfer = async (s,fr,to,tok,amt) => {
  const r = await interact('POST', `${address}/transfer/${s}/?from=${fr}&to=${to}&token=${tok}&amount=${amt}`)
  console.log(r)
  return r
}

const resetServer = async () => {
  const r = await interact('POST', `${address}/reset`)
  console.log(r)
  return r
}

const ping = async () => {
  const r = await interact(`GET`, `${address}/ping`)
  console.log(r)
  return r;
}

const clientMethods = {
  "getStates" : getStates,
  "getStatus" : getStatus,
  "getActions" : getActions,
  "load" : load,
  "init" : init,
  "respondWithVal" : respondWithVal,
  "ping" : ping,
  "waitForPort" : waitForPort,
  "initFor" : initFor,
  "getStateLocals" : getStateLocals,
  "getStateGlobals" : getStateGlobals,
  "getEdges" : getEdges,
  "resetServer" : resetServer,
}

const interpCommand = async (comm) => {
  const fnstring = comm[0];
  const fnparams = comm.slice(1);
  const fn = clientMethods[fnstring];
  const r = await fn.apply(null, fnparams);
  console.log(r)
  return r;
}

const interp = async (comms) => {
  for (const co of comms) {
    await interpCommand(co);
  }
}

export {
  getStates,
  getStatus,
  getActions,
  load,
  init,
  respondWithVal,
  ping,
  waitForPort,
  initFor,
  getStateLocals,
  getStateGlobals,
  getEdges,
  resetServer,
  interpCommand,
  interp,
  getLoc,
  newAccount,
  transfer,
  newToken
};
