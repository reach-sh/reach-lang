// Automatically generated with Reach 0.1.10 (f1275a40*)
/* eslint-disable */
export const _version = '0.1.10';
export const _versionHash = '0.1.10 (f1275a40*)';
export const _backendVersion = 13;

export function getExports(s) {
  const stdlib = s.reachStdlib;
  return {
    };
  };
export function _getEvents(s) {
  const stdlib = s.reachStdlib;
  return {
    };
  };
export function _getViews(s, viewlib) {
  const stdlib = s.reachStdlib;
  const ctc0 = stdlib.T_Address;
  const ctc1 = stdlib.T_UInt;
  
  return {
    infos: {
      },
    views: {
      1: [ctc0, ctc1],
      2: [ctc0, ctc1]
      }
    };
  
  };
export function _getMaps(s) {
  const stdlib = s.reachStdlib;
  const ctc0 = stdlib.T_Tuple([]);
  return {
    mapDataTy: ctc0
    };
  };
export async function Alice(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for Alice expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Alice expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '128'));
  const ctc1 = stdlib.T_UInt;
  const ctc2 = stdlib.T_Address;
  
  
  const v64 = stdlib.protect(ctc0, interact.info, 'for Alice\'s interact field info');
  const v65 = stdlib.protect(ctc1, interact.request, 'for Alice\'s interact field request');
  
  const txn1 = await (ctc.sendrecv({
    args: [v65],
    evt_cnt: 1,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./examples/overview/index.rsh:17:5:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./examples/overview/index.rsh:17:5:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v69], secs: v71, time: v70, didSend: v28, from: v68 } = txn1;
      
      ;
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc1],
    waitIfNotPresent: false
    }));
  const {data: [v69], secs: v71, time: v70, didSend: v28, from: v68 } = txn1;
  ;
  const txn2 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: undefined /* mto */,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v76, time: v75, didSend: v37, from: v74 } = txn2;
  ;
  const txn3 = await (ctc.sendrecv({
    args: [v68, v69, v64],
    evt_cnt: 1,
    funcNum: 2,
    lct: v75,
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./examples/overview/index.rsh:27:5:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v82], secs: v84, time: v83, didSend: v46, from: v81 } = txn3;
      
      ;
      const v85 = stdlib.addressEq(v68, v81);
      ;
      sim_r.txns.push({
        amt: v69,
        kind: 'from',
        to: v68,
        tok: undefined /* Nothing */
        });
      sim_r.txns.push({
        kind: 'halt',
        tok: undefined /* Nothing */
        })
      sim_r.isHalt = true;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc2, ctc1, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v82], secs: v84, time: v83, didSend: v46, from: v81 } = txn3;
  ;
  const v85 = stdlib.addressEq(v68, v81);
  stdlib.assert(v85, {
    at: './examples/overview/index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  ;
  return;
  
  
  
  
  
  
  };
export async function Bob(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for Bob expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Bob expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Null;
  const ctc2 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '128'));
  const ctc3 = stdlib.T_Address;
  
  
  const txn1 = await (ctc.recv({
    didSend: false,
    evt_cnt: 1,
    funcNum: 0,
    out_tys: [ctc0],
    timeoutAt: undefined /* mto */,
    waitIfNotPresent: false
    }));
  const {data: [v69], secs: v71, time: v70, didSend: v28, from: v68 } = txn1;
  ;
  stdlib.protect(ctc1, await interact.want(v69), {
    at: './examples/overview/index.rsh:21:18:application',
    fs: ['at ./examples/overview/index.rsh:20:9:application call to [unknown function] (defined at: ./examples/overview/index.rsh:20:13:function exp)'],
    msg: 'want',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v68, v69],
    evt_cnt: 0,
    funcNum: 1,
    lct: v70,
    onlyIf: true,
    out_tys: [],
    pay: [v69, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [], secs: v76, time: v75, didSend: v37, from: v74 } = txn2;
      
      sim_r.txns.push({
        amt: v69,
        kind: 'to',
        tok: undefined /* Nothing */
        });
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc3, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v76, time: v75, didSend: v37, from: v74 } = txn2;
  ;
  const txn3 = await (ctc.recv({
    didSend: false,
    evt_cnt: 1,
    funcNum: 2,
    out_tys: [ctc2],
    timeoutAt: undefined /* mto */,
    waitIfNotPresent: false
    }));
  const {data: [v82], secs: v84, time: v83, didSend: v46, from: v81 } = txn3;
  ;
  const v85 = stdlib.addressEq(v68, v81);
  stdlib.assert(v85, {
    at: './examples/overview/index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v82), {
    at: './examples/overview/index.rsh:32:17:application',
    fs: ['at ./examples/overview/index.rsh:31:9:application call to [unknown function] (defined at: ./examples/overview/index.rsh:31:13:function exp)'],
    msg: 'got',
    who: 'Bob'
    });
  
  return;
  
  
  
  
  
  
  };
const _ALGO = {
  ABI: {
    impure: [],
    pure: [],
    sigs: []
    },
  appApproval: `BiAEAAECICYCAQAAIjUAMRhBAVApZEkiWzUBgQhbNQI2GgAXSUEAByI1BCM1BgA2GgIXNQQ2GgM2GgEXSSMMQACRSSQMQABHJBJEJDQBEkQ0BEkiEkw0AhIRRChkSTUDVwAgNf9JNQU1/oAEXpV+NDT+ULA0/zEAEkSxIrIBNAMlW7III7IQNP+yB7NCAIVIIzQBEkQ0BEkiEkw0AhIRRChkSTUDSVcAIDX/JVs1/oAEmouRdLA0/ogAsjT/NP4WUChLAVcAKGdIJDUBMgY1AkIAXUgiNAESRDQESSISTDQCEhFESTUFFzX/gASCxGH+NP8WULCBoI0GiABxMQA0/xZQKEsBVwAoZ0gjNQEyBjUCQgAcMRmBBRJEsSKyASKyCCOyEDIJsgkyCrIHs0IABTEZIhJEKTQBFjQCFlBnNAZBAAqABBUffHU0B1CwNABJIwgyBBJEMRYSRCNDMRkiEkRC/98iNQEiNQJC/8M0AElKIwg1ADgHMgoSRDgQIxJEOAgSRIk=`,
  appClear: `Bg==`,
  companionInfo: null,
  extraPages: 0,
  mapDataKeys: 0,
  mapDataSize: 0,
  stateKeys: 1,
  stateSize: 40,
  unsupported: [],
  version: 10,
  warnings: []
  };
const _ETH = {
  ABI: `[
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T2",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "stateMutability": "payable",
    "type": "constructor"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "msg",
        "type": "uint256"
      }
    ],
    "name": "ReachError",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "_who",
        "type": "address"
      },
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v69",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T2",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e0",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "_who",
        "type": "address"
      },
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T4",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e1",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "_who",
        "type": "address"
      },
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "components": [
                  {
                    "internalType": "bytes32",
                    "name": "elem0",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem1",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem2",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem3",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T5",
                "name": "v82",
                "type": "tuple"
              }
            ],
            "internalType": "struct T6",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T7",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e2",
    "type": "event"
  },
  {
    "stateMutability": "payable",
    "type": "fallback"
  },
  {
    "inputs": [],
    "name": "_reachCreationTime",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "_reachCurrentState",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "",
        "type": "bytes"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "_reachCurrentTime",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T4",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_m1",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "components": [
                  {
                    "internalType": "bytes32",
                    "name": "elem0",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem1",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem2",
                    "type": "bytes32"
                  },
                  {
                    "internalType": "bytes32",
                    "name": "elem3",
                    "type": "bytes32"
                  }
                ],
                "internalType": "struct T5",
                "name": "v82",
                "type": "tuple"
              }
            ],
            "internalType": "struct T6",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T7",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_m2",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "stateMutability": "payable",
    "type": "receive"
  }
]`,
  Bytecode: `0x60806040526040516109cc3803806109cc833981016040819052610022916101a1565b6000805543600355604080513381528251602080830191909152830151518183015290517f28822ae872174fb8917549901c639f920e5c2ef0fb881ea78a94dee578586e9d9181900360600190a161007c341560076100df565b6040805180820182526000602080830182815233808552868301515182526001938490554390935584518083019390935251828501528351808303850181526060909201909352805191926100d79260029290910190610108565b505050610279565b816101045760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b8280546101149061023f565b90600052602060002090601f016020900481019282610136576000855561017c565b82601f1061014f57805160ff191683800117855561017c565b8280016001018555821561017c579182015b8281111561017c578251825591602001919060010190610161565b5061018892915061018c565b5090565b5b80821115610188576000815560010161018d565b60008183036040808212156101b557600080fd5b80518082016001600160401b0380821183831017156101e457634e487b7160e01b600052604160045260246000fd5b818452865183526020601f19860112156101fd57600080fd5b83519450602085019150848210818311171561022957634e487b7160e01b600052604160045260246000fd5b5090915260209384015182529283015250919050565b600181811c9082168061025357607f821691505b60208210810361027357634e487b7160e01b600052602260045260246000fd5b50919050565b610744806102886000396000f3fe60806040526004361061004b5760003560e01c80631e93b0f1146100545780632c10a15914610078578063832307571461008b5780638dc34f55146100a0578063ab53f2c6146100b357005b3661005257005b005b34801561006057600080fd5b506003545b6040519081526020015b60405180910390f35b6100526100863660046105a4565b6100d6565b34801561009757600080fd5b50600154610065565b6100526100ae3660046105bc565b610259565b3480156100bf57600080fd5b506100c861040c565b60405161006f9291906105ce565b6100e660016000541460096104a9565b610100813515806100f957506001548235145b600a6104a9565b6000808055600280546101129061062b565b80601f016020809104026020016040519081016040528092919081815260200182805461013e9061062b565b801561018b5780601f106101605761010080835404028352916020019161018b565b820191906000526020600020905b81548152906001019060200180831161016e57829003601f168201915b50505050508060200190518101906101a3919061065f565b90507f400d21ea4e4a5e28b4ae5f0f476c201fc8036473fcf7c8cd252f38698020b4f133836040516101d69291906106d1565b60405180910390a16101ef8160200151341460086104a9565b6040805180820182526000808252602080830182815285516001600160a01b031680855286830151825260029384905543600155855180840191909152905181860152845180820386018152606090910190945283519293610253939101906104ce565b50505050565b610269600260005414600d6104a9565b6102838135158061027c57506001548235145b600e6104a9565b6000808055600280546102959061062b565b80601f01602080910402602001604051908101604052809291908181526020018280546102c19061062b565b801561030e5780601f106102e35761010080835404028352916020019161030e565b820191906000526020600020905b8154815290600101906020018083116102f157829003601f168201915b5050505050806020019051810190610326919061065f565b604080513381528435602080830191909152850135818301529084013560608083019190915284013560808083019190915284013560a08201529091507fc65c1596dd699092d6d17a921785a8b5bc1a13a6e25c5b9fa99d25b7613375609060c00160405180910390a161039c3415600b6104a9565b80516103b4906001600160a01b03163314600c6104a9565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156103f1573d6000803e3d6000fd5b506000808055600181905561040890600290610552565b5050565b6000606060005460028080546104219061062b565b80601f016020809104026020016040519081016040528092919081815260200182805461044d9061062b565b801561049a5780601f1061046f5761010080835404028352916020019161049a565b820191906000526020600020905b81548152906001019060200180831161047d57829003601f168201915b50505050509050915091509091565b816104085760405163100960cb60e01b81526004810182905260240160405180910390fd5b8280546104da9061062b565b90600052602060002090601f0160209004810192826104fc5760008555610542565b82601f1061051557805160ff1916838001178555610542565b82800160010185558215610542579182015b82811115610542578251825591602001919060010190610527565b5061054e92915061058f565b5090565b50805461055e9061062b565b6000825580601f1061056e575050565b601f01602090049060005260206000209081019061058c919061058f565b50565b5b8082111561054e5760008155600101610590565b6000604082840312156105b657600080fd5b50919050565b600060a082840312156105b657600080fd5b82815260006020604081840152835180604085015260005b81811015610602578581018301518582016060015282016105e6565b81811115610614576000606083870101525b50601f01601f191692909201606001949350505050565b600181811c9082168061063f57607f821691505b6020821081036105b657634e487b7160e01b600052602260045260246000fd5b60006040828403121561067157600080fd5b6040516040810181811067ffffffffffffffff821117156106a257634e487b7160e01b600052604160045260246000fd5b60405282516001600160a01b03811681146106bc57600080fd5b81526020928301519281019290925250919050565b6001600160a01b0383168152813560208083019190915260608201908301358015158082146106ff57600080fd5b8060408501525050939250505056fea2646970667358221220049c0270279057e1bd133fe2b4507b468e49581a1ae26cb337c1ec312bef675c64736f6c634300080d0033`,
  BytecodeLen: 2508,
  Which: `oD`,
  version: 7,
  views: {
    }
  };
export const _stateSourceMap = {
  1: {
    at: './examples/overview/index.rsh:18:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  2: {
    at: './examples/overview/index.rsh:23:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  3: {
    at: './examples/overview/index.rsh:29:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    }
  };
export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };
export const _Participants = {
  "Alice": Alice,
  "Bob": Bob
  };
export const _APIs = {
  };
