import fetch from 'node-fetch';
import waitPort from 'wait-port';

const hostname = "http://localhost"
const port = 3001
const address = `${hostname}:${port}`

// wait for the server port to open
const waitForPort = async () => {
  const params = {
    port: port
  }
  const r = await waitPort(params)
  console.log(r)
}

class HTTPResponseError extends Error {
  response: any;

  constructor(response: any) {
    super(`HTTP Error Response: ${response.status} ${response.statusText}`);
    this.response = response;
	}
}

const checkStatus = (response: any) => {
	if (response.ok) {
		// response.status >= 200 && response.status < 300
		return response.json();
	} else {
		throw new HTTPResponseError(response);
	}
}

// helper to make http requests
async function interact(method = 'GET', url = '', data = {}) {
  const response = await fetch(url, {
    method: method,
    // cache: 'no-cache',
    headers: {
      'Content-Type': 'application/json'
    },
    redirect: 'follow',
    referrerPolicy: 'no-referrer'
  });
  try {
  	return checkStatus(response);
  } catch (error) {
  	console.error(await error.response.text());
  }
}

// returns: a mapping from state id to state info
// state info is an association list that contains who created that state
// and what action they ran
  // 0: Array(2)
  //   0: -1
  //   1: {tag: 'A_TieBreak', contents: Array(2)}
  // 1: Array(2)
  //   0: 0
  //   1: {tag: 'A_Interact', contents: Array(5)}
const getStates = async () => {
  const r = await interact(`GET`, `${address}/states`)
  console.log(r)
  return r;
}

// edges of the DAG state graph, represented as a list of pairs
// of numbers
// returns: '[[6,7],[5,6],[4,5],[3,4],[2,3],[1,2],[0,1]]'
const getEdges = async () => {
  const r = await interact(`GET`, `${address}/edges`)
  console.log(r)
  return r;
}

// returns the status of the "active" actor
// "active" means took an action most recently
// returns: "Initial" or "Running" or "Done"
const getStatus = async () => {
  const r = await interact(`GET`, `${address}/status`)
  console.log(r)
  return r;
}

// get the list of available actions
// at state s (integer)
// for actor a (integer)
// returns: '[5,{"tag":"A_Receive","contents":1}]'
// here 5 is the state id
// 1 is the phase id
// the structure of each action is different
async function getActions(s: number,a: number) {
  const r = await interact(`GET`, `${address}/actions/${s}/${a}`)
  console.log(JSON.stringify(r))
  return r;
}

// JSON dump of global Simulator info
// at state s (integer)
// returns:
// {
//   "e_ledger": {
//     "0": {
//       "-1": 0
//     },
//     "1": {
//       "-1": 0
//     },
//     "-1": {
//       "-1": 0
//     }
//   },
//   "e_messages": {
//     "0": {
//       "tag": "Fixed",
//       "contents": [
//         0,
//         "Message"
//       ]
//     }
//   },
//   "e_nwsecs": 3,
//   "e_naccid": 2,
//   "e_linstate": [],
//   "e_ntok": 0,
//   "e_nactorid": 2,
//   "e_partacts": {
//     "Bob": 1,
//     "Alice": 0
//   },
//   "e_nwtime": 3
// }
async function getStateGlobals(s: number) {
  const r = await interact(`GET`, `${address}/global/${s}`)
  console.log(r)
  return r;
}

// JSON dump of local Simulator info
// at state s (integer)
// returns:
// {
//   "l_locals": {
//     "0": {
//       "l_phase": 2,
//       "l_ivd": {
//         "getHand": {
//           "tag": "IT_Fun",
//           "contents": [
//             [],
//             {
//               "tag": "T_UInt"
//             }
//           ]
//         },
//         "seeOutcome": {
//           "tag": "IT_Fun",
//           "contents": [
//             [
//               {
//                 "tag": "T_UInt"
//               }
//             ],
//             {
//               "tag": "T_Null"
//             }
//           ]
//         }
//       },
//       "l_who": "Alice",
//       "l_store": [
//         [
//           "didPublish/27",
//           {
//             "tag": "V_Bool",
//             "contents": true
//           }
//         ],
//         [
//           "v58",
//           {
//             "tag": "V_UInt",
//             "contents": 2
//           }
//         ],
//         [
//           "Alice/59",
//           {
//             "tag": "V_Address",
//             "contents": 0
//           }
//         ],
//         [
//           "handAlice/60",
//           {
//             "tag": "V_UInt",
//             "contents": 2
//           }
//         ],
//         [
//           "thisConsensusTime/61",
//           {
//             "tag": "V_UInt",
//             "contents": 1
//           }
//         ],
//         [
//           "thisConsensusSecs/62",
//           {
//             "tag": "V_UInt",
//             "contents": 1
//           }
//         ]
//       ],
//       "l_livs": {},
//       "l_ks": "PS_Suspend",
//       "l_acct": 0
//     },
//     "1": {
//       "l_phase": 1,
//       "l_ivd": {
//         "getHand": {
//           "tag": "IT_Fun",
//           "contents": [
//             [],
//             {
//               "tag": "T_UInt"
//             }
//           ]
//         },
//         "seeOutcome": {
//           "tag": "IT_Fun",
//           "contents": [
//             [
//               {
//                 "tag": "T_UInt"
//               }
//             ],
//             {
//               "tag": "T_Null"
//             }
//           ]
//         }
//       },
//       "l_who": "Bob",
//       "l_store": [
//         [
//           "didPublish/27",
//           {
//             "tag": "V_Bool",
//             "contents": false
//           }
//         ],
//         [
//           "Alice/59",
//           {
//             "tag": "V_Address",
//             "contents": 0
//           }
//         ],
//         [
//           "handAlice/60",
//           {
//             "tag": "V_UInt",
//             "contents": 2
//           }
//         ],
//         [
//           "thisConsensusTime/61",
//           {
//             "tag": "V_UInt",
//             "contents": 2
//           }
//         ],
//         [
//           "thisConsensusSecs/62",
//           {
//             "tag": "V_UInt",
//             "contents": 2
//           }
//         ]
//       ],
//       "l_livs": {},
//       "l_ks": "PS_Suspend",
//       "l_acct": 1
//     },
//     "-1": {
//       "l_phase": 2,
//       "l_ivd": {},
//       "l_who": null,
//       "l_store": [
//         [
//           "didPublish/27",
//           {
//             "tag": "V_Bool",
//             "contents": false
//           }
//         ],
//         [
//           "Alice/59",
//           {
//             "tag": "V_Address",
//             "contents": 0
//           }
//         ],
//         [
//           "handAlice/60",
//           {
//             "tag": "V_UInt",
//             "contents": 2
//           }
//         ],
//         [
//           "thisConsensusTime/61",
//           {
//             "tag": "V_UInt",
//             "contents": 0
//           }
//         ],
//         [
//           "thisConsensusSecs/62",
//           {
//             "tag": "V_UInt",
//             "contents": 0
//           }
//         ]
//       ],
//       "l_livs": {},
//       "l_ks": "PS_Suspend",
//       "l_acct": -1
//     }
//   },
//   "l_curr_actor_id": 1
// }
async function getStateLocals(s: number) {
  const r = await interact(`GET`, `${address}/local/${s}`)
  console.log(r)
  return r;
}

// returns a ":" delimited string
// returns: for example: /Users/chike/reach-lang/examples/api-full/index.rsh:27:5:dot
// where 27 is the line number
// 5 is the column number
// for actor a (integer)
// at state s (integer)
async function getLoc(s: number,a: number) {
  const r = await interact(`GET`, `${address}/locs/${s}/${a}`)
  console.log(r)
  return r;
}

// load the program (Reach source code)
const load = async () => {
  const r = await interact('POST', `${address}/load`)
  console.log(r)
  return r;
}

// initialize the program for the Consensus
const init = async () => {
  const r = await interact('POST', `${address}/init`)
  console.log(r)
  return r
}

// at state s (integer)
// perform action a (integer)
// with a value v (Simulator.Core.DLVal)
// as an actor w (integer).
// by default or by passing 'false' the actor is unchanged.
// t provides the type of the value
// for example
// const a = {
//   tag: 'V_Tuple',
//   contents: [
//     {tag: 'V_UInt',
//      contents: 4444
//     },
//     {tag: 'V_UInt',
//      contents: 0}
//   ]}
// c.respondWithVal(2,2,0,a)
// this is the most interesting endpoint
// full documentation with all the values/types should exist elsewhere
// but there are numerous examples in ~/reach-lang/examples/simulator-*
const respondWithVal = async (s: number,a: number,v: any,w: any = false,t='number') => {
  const who = (w || w === 0) ? `&who=${w}` : ``
  const r = await interact('POST', `${address}/states/${s}/actions/${a}/?data=${v}${who}&type=${t}`)
  console.log(r)
  return r
}

// initialize the program for
// actor a (integer)
// at state s (integer)
// liv is the interact environment. it is a JSON object
// optionally, provide an account id acc (integer)
// for example :
// await c.initFor(0,0,JSON.stringify({'wager':{'tag':'V_UInt','contents':10}}))
const initFor = async (s: number,a: number,liv="{}",acc: any = false) => {
  const accParam = (acc || acc === 0) ? `&accountId=${acc}` : ``
  let livS = liv
  if (
    typeof liv === 'object' &&
    !Array.isArray(liv) &&
    liv !== null
  ) {
    livS = JSON.stringify(liv)
  }
  const r = await interact('POST', `${address}/init/${a}/${s}/?liv=${livS}${accParam}`)
  console.log(r)
  return r
}

// check if there are initialization interact details needed
// to run the program
// for actor a (integer)
const initDetails = async (a: number) => {
  const r = await interact('GET', `${address}/init_details/${a}`)
  console.log(r)
  return r
}

// returns the state category graph for the program
// for example:
// {
//   "0":"Consensus",
//   "1":"Local",
//   "2":"Local",
//   "3":"Local",
//   "4":"Consensus",
//   "5":"Local",
//   "6":"Local",
//   "7":"Local",
//   "8":"Consensus",
//   "9":"Local",
//   "10":"Local",
//   "11":"Local",
//   "12":"Local"
// }
const catGraph = async () => {
  const r = await interact('GET', `${address}/catgraph`)
  console.log(r)
  return r
}

const dotGraph = async () => {
  const r = await interact('GET', `${address}/dotstategraph`)
  console.log(r)
  return r
}

// create a new account
// at state s (integer)
const newAccount = async (s: number) => {
  const r = await interact('POST', `${address}/accounts/new/${s}`)
  console.log(r)
  return r
}

// create a new currency
// at state s (integer)
const newToken = async (s: number) => {
  const r = await interact('POST', `${address}/tokens/new/${s}`)
  console.log(r)
  return r
}

// perform a funds transfer
// at state s (integer)
// from account fr (integer)
// to account to (integer)
// for currency tok (integer)
// for amount amt (integer)
const transfer = async (s: number,fr: number,to: number,tok: number,amt: number) => {
  const r = await interact('POST', `${address}/transfer/${s}/?from=${fr}&to=${to}&token=${tok}&amount=${amt}`)
  console.log(r)
  return r
}

// resets the Simulator server entirely
// all state is deleted
const resetServer = async () => {
  const r = await interact('POST', `${address}/reset`)
  console.log(r)
  return r
}

// get the APIs for the current program
const getAPIs = async () => {
  const r = await interact('GET', `${address}/apis`)
  console.log(r)
  return r
}

// get the Views for the current program
// at state s (integer)
const getViews = async (s: number) => {
  const r = await interact('GET', `${address}/views/${s}`)
  console.log(r)
  return r
}

// get the State Graph for the current program
const getStateGraph = async () => {
  const r = await interact('GET', `${address}/graph`)
  console.log(r)
  return r
}

const apiCall = async (a: number,s: number,v: any,t='number') => {
  const r = await interact('POST', `${address}/api_call/${a}/${s}/?data=${v}&type=${t}`)
  console.log(r)
  return r
}

const viewCall = async (a: number,s: number,v: any,t='number') => {
  const r = await interact('POST', `${address}/view_call/${a}/${s}/?data=${v}&type=${t}`)
  console.log(r)
  return r
}

const passTime = async (s: number,n: number) => {
  const r = await interact('POST', `${address}/wait/${s}/${n}`)
  console.log(r)
  return r
}

const forceTimeout = async (s: number) => {
  const r = await interact('POST', `${address}/timeout/${s}`)
  console.log(r)
  return r
}

// ping the server for a friendly greeting ^_^
const ping = async () => {
  const r = await interact(`GET`, `${address}/ping`)
  console.log(r)
  return r;
}

// we expose certain methods to be available in a user provided
// Simulation "script" sequence of commands
const clientMethods: Record<string, (...args: any[]) => Promise<any>> = {
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
  "newAccount" : newAccount,
  "newToken" : newToken,
  "apiCall" : apiCall
}

// run a single Simulation "script" command
const interpCommand = async (comm: any) => {
  const fnstring: string = comm[0];
  const fnparams = comm.slice(1);
  const fn = clientMethods[fnstring];
  const r = await fn.apply(null, fnparams);
  console.log(r)
  return r;
}

// run an entire JSON list of commands
const interp = async (comms: any) => {
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
  newToken,
  initDetails,
  catGraph,
  getAPIs,
  apiCall,
  getViews,
  viewCall,
  getStateGraph,
  passTime,
  dotGraph,
  forceTimeout
};
