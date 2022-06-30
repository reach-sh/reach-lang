// Automatically generated with Reach 0.1.11
/* eslint-disable */
export const _version = '0.1.11';
export const _versionHash = '0.1.11';
export const _backendVersion = 16;

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
  const ctc0 = stdlib.T_UInt;

  return {
    infos: {
      },
    views: {
      3: [ctc0, ctc0]
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
export async function D(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for D expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for D expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Tuple([ctc0]);


  const v98 = stdlib.protect(ctc0, interact.y, 'for D\'s interact field y');

  const txn1 = await (ctc.sendrecv({
    args: [v98],
    evt_cnt: 1,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./child.rsh:14:5:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./child.rsh:14:5:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };


      const {data: [v102], secs: v104, time: v103, didSend: v27, from: v101 } = txn1;

      ;
      const v105 = stdlib.checkedBigNumberify('./child.rsh:16:28:decimal', stdlib.UInt_max, '0');
      const v106 = v103;

      if (await (async () => {
        const v112 = stdlib.lt(v105, stdlib.checkedBigNumberify('./child.rsh:18:16:decimal', stdlib.UInt_max, '2'));

        return v112;})()) {
        sim_r.isHalt = false;
        }
      else {
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined /* Nothing */
          })
        sim_r.isHalt = true;
        }
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v102], secs: v104, time: v103, didSend: v27, from: v101 } = txn1;
  ;
  let v105 = stdlib.checkedBigNumberify('./child.rsh:16:28:decimal', stdlib.UInt_max, '0');
  let v106 = v103;

  while (await (async () => {
    const v112 = stdlib.lt(v105, stdlib.checkedBigNumberify('./child.rsh:18:16:decimal', stdlib.UInt_max, '2'));

    return v112;})()) {
    const txn2 = await (ctc.recv({
      didSend: false,
      evt_cnt: 1,
      funcNum: 2,
      out_tys: [ctc1],
      timeoutAt: undefined /* mto */,
      waitIfNotPresent: false
      }));
    const {data: [v123], secs: v125, time: v124, didSend: v69, from: v122 } = txn2;
    undefined /* setApiDetails */;
    const v127 = v123[stdlib.checkedBigNumberify('./child.rsh:19:10:spread', stdlib.UInt_max, '0')];
    ;
    const v130 = stdlib.add(v105, v102);
    const v131 = stdlib.add(v130, v127);
    await txn2.getOutput('f', 'v131', ctc0, v131);
    const v138 = stdlib.add(v105, stdlib.checkedBigNumberify('./child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
    const cv105 = v138;
    const cv106 = v124;

    v105 = cv105;
    v106 = cv106;

    continue;

    }
  return;


  };
export async function _f3(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for _f3 expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for _f3 expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Tuple([ctc0]);
  const ctc2 = stdlib.T_Null;


  const [v102, v105] = await ctc.getState(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '3'), [ctc0, ctc0]);
  const v115 = stdlib.protect(ctc1, await interact.in(), {
    at: './child.rsh:1:23:application',
    fs: ['at ./child.rsh:19:20:application call to [unknown function] (defined at: ./child.rsh:19:20:function exp)', 'at ./child.rsh:19:20:application call to [unknown function] (defined at: ./child.rsh:19:20:function exp)'],
    msg: 'in',
    who: 'f'
    });

  const txn1 = await (ctc.sendrecv({
    args: [v102, v105, v115],
    evt_cnt: 1,
    funcNum: 2,
    lct: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./child.rsh:20:16:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };


      const {data: [v123], secs: v125, time: v124, didSend: v69, from: v122 } = txn1;

      sim_r.txns.push({
        kind: 'api',
        who: "f"
        });
      const v127 = v123[stdlib.checkedBigNumberify('./child.rsh:19:10:spread', stdlib.UInt_max, '0')];
      ;
      const v130 = stdlib.add(v105, v102);
      const v131 = stdlib.add(v130, v127);
      const v132 = await txn1.getOutput('f', 'v131', ctc0, v131);

      const v138 = stdlib.add(v105, stdlib.checkedBigNumberify('./child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
      const v177 = v138;
      const v179 = stdlib.lt(v138, stdlib.checkedBigNumberify('./child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
      if (v179) {
        sim_r.isHalt = false;
        }
      else {
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined /* Nothing */
          })
        sim_r.isHalt = true;
        }
      return sim_r;
      }),
    soloSend: false,
    timeoutAt: undefined /* mto */,
    tys: [ctc0, ctc0, ctc1],
    waitIfNotPresent: false
    }));
  const {data: [v123], secs: v125, time: v124, didSend: v69, from: v122 } = txn1;
  undefined /* setApiDetails */;
  const v127 = v123[stdlib.checkedBigNumberify('./child.rsh:19:10:spread', stdlib.UInt_max, '0')];
  ;
  const v130 = stdlib.add(v105, v102);
  const v131 = stdlib.add(v130, v127);
  const v132 = await txn1.getOutput('f', 'v131', ctc0, v131);
  if (v69) {
    stdlib.protect(ctc2, await interact.out(v123, v132), {
      at: './child.rsh:19:11:application',
      fs: ['at ./child.rsh:19:11:application call to [unknown function] (defined at: ./child.rsh:19:11:function exp)', 'at ./child.rsh:21:10:application call to "k" (defined at: ./child.rsh:20:23:function exp)', 'at ./child.rsh:20:23:application call to [unknown function] (defined at: ./child.rsh:20:23:function exp)'],
      msg: 'out',
      who: 'f'
      });
    }
  else {
    }

  const v138 = stdlib.add(v105, stdlib.checkedBigNumberify('./child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
  const v177 = v138;
  const v179 = stdlib.lt(v138, stdlib.checkedBigNumberify('./child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
  if (v179) {
    return;
    }
  else {
    return;
    }


  };
export async function f(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for f expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for f expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const step = await ctc.getCurrentStep()
  stdlib.assert(step == 3, 'API called in the wrong state. Currently in state: ' + step + ', expected:  [3]');
  if (step == 3) {return _f3(ctcTop, interact);}
  };
const _ALGO = {
  ABI: {
    impure: [`f(uint64)uint64`],
    pure: [],
    sigs: [`f(uint64)uint64`]
    },
  appApproval: `BiAFAAECCAMmAgABACI1ADEYQQFEKGRJIls1ASVbNQI2GgAXSUEAEyI1BCM1BoHpiox/EkQ2GgFCABY2GgIXNQQ2GgM2GgEXSSQMQABeJBJEIQQ0ARJENARJIhJMNAISEUQpZEk1A0kiWzX/JVs1/kk1BTX9gAQchk/lNP1QsIAIAAAAAAAAAIM0/jT/CDT9FwgWULA0/jT/CDT9FwgWNQc0/zT+IwgyBkIAMyISRIGgjQaIAKwiNAESRDQESSISTDQCEhFESTUFFzX/gASCxGH+NP8WULA0/yIyBkIAADX/Nf41/TT+JAxBABo0/RY0/hZQKUsBVwAQZ0ghBDUBMgY1AkIAH0IAADEZgQUSRLEisgEisggjshAyCbIJMgqyB7NCAAUxGSISRCg0ARY0AhZQZzQGQQAKgAQVH3x1NAdQsDQASSMIMgQSRDEWEkQjQzEZIhJEQv/fIjUBIjUCQv/DNABJSiMINQA4BzIKEkQ4ECMSRDgIEkSJ`,
  appClear: `Bg==`,
  companionInfo: null,
  extraPages: 0,
  mapDataKeys: 0,
  mapDataSize: 0,
  stateKeys: 1,
  stateSize: 16,
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
                "name": "v102",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T3",
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
                "name": "v102",
                "type": "uint256"
              }
            ],
            "internalType": "struct T0",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T3",
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
            "components": [
              {
                "components": [
                  {
                    "internalType": "uint256",
                    "name": "elem0",
                    "type": "uint256"
                  }
                ],
                "internalType": "struct T5",
                "name": "v123",
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
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "v0",
        "type": "uint256"
      }
    ],
    "name": "_reach_oe_v131",
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
            "components": [
              {
                "components": [
                  {
                    "internalType": "uint256",
                    "name": "elem0",
                    "type": "uint256"
                  }
                ],
                "internalType": "struct T5",
                "name": "v123",
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
    "inputs": [
      {
        "internalType": "uint256",
        "name": "_a0",
        "type": "uint256"
      }
    ],
    "name": "f",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "stateMutability": "payable",
    "type": "receive"
  }
]`,
  Bytecode: `0x608060405260405162000b3638038062000b3683398101604081905262000026916200028e565b6000805543600355604080513381528251602080830191909152830151518183015290517f28822ae872174fb8917549901c639f920e5c2ef0fb881ea78a94dee578586e9d9181900360600190a16200008234156007620000b7565b6200008c62000171565b602080830151518251528181018051600090525143910152620000af81620000e1565b50506200036c565b81620000dd5760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b60208101515160021115620001565760408051808201825260008082526020808301828152855151808552868301515182526003909355436001558451808301939093525182850152835180830385018152606090920190935280519192620001519260029290910190620001ae565b505050565b600080805560018190556200016e906002906200023d565b50565b6040805160608101825260009181019182529081908152602001620001a9604051806040016040528060008152602001600081525090565b905290565b828054620001bc9062000330565b90600052602060002090601f016020900481019282620001e057600085556200022b565b82601f10620001fb57805160ff19168380011785556200022b565b828001600101855582156200022b579182015b828111156200022b5782518255916020019190600101906200020e565b506200023992915062000277565b5090565b5080546200024b9062000330565b6000825580601f106200025c575050565b601f0160209004906000526020600020908101906200016e91905b5b8082111562000239576000815560010162000278565b6000818303604080821215620002a357600080fd5b80518082016001600160401b038082118383101715620002d357634e487b7160e01b600052604160045260246000fd5b818452865183526020601f1986011215620002ed57600080fd5b8351945060208501915084821081831117156200031a57634e487b7160e01b600052604160045260246000fd5b5090915260209384015182529283015250919050565b600181811c908216806200034557607f821691505b6020821081036200036657634e487b7160e01b600052602260045260246000fd5b50919050565b6107ba806200037c6000396000f3fe60806040526004361061004b5760003560e01c80631e93b0f114610054578063763740c414610078578063832307571461008b578063ab53f2c6146100a0578063b3de648b146100c357005b3661005257005b005b34801561006057600080fd5b506003545b6040519081526020015b60405180910390f35b6100526100863660046105ab565b6100d6565b34801561009757600080fd5b50600154610065565b3480156100ac57600080fd5b506100b5610101565b60405161006f9291906105c3565b6100656100d1366004610620565b61019e565b6040805160208101909152600081526100fd6100f7368490038401846106a1565b826101e7565b5050565b600060606000546002808054610116906106f8565b80601f0160208091040260200160405190810160405280929190818152602001828054610142906106f8565b801561018f5780601f106101645761010080835404028352916020019161018f565b820191906000526020600020905b81548152906001019060200180831161017257829003601f168201915b50505050509050915091509091565b60006101b66040518060200160405280600081525090565b6101be610473565b60408051808201909152602080820186815282528201526101df81836101e7565b505192915050565b6101f760036000541460096103c3565b815161021290158061020b57508251600154145b600a6103c3565b600080805560028054610224906106f8565b80601f0160208091040260200160405190810160405280929190818152602001828054610250906106f8565b801561029d5780601f106102725761010080835404028352916020019161029d565b820191906000526020600020905b81548152906001019060200180831161028057829003601f168201915b50505050508060200190518101906102b5919061072c565b60408051338152855160208083019190915286015151518183015290519192507face91d8311c1d42194ceea7a64ba4f6f1d530122abc027407da2595ed96d68e1919081900360600190a161030c341560086103c3565b60208084015151518251918301517fb1885c0fe128efbc823b7323770c75a9e7e8e72714fc480be1ce86ddb4b4304f926103459161075e565b61034f919061075e565b60405190815260200160405180910390a1602080840151515182519183015190916103799161075e565b610383919061075e565b825261038d6104a5565b815181515260208201516103a39060019061075e565b60208083018051929092529051439101526103bd816103e8565b50505050565b816100fd5760405163100960cb60e01b81526004810182905260240160405180910390fd5b6020810151516002111561045a576040805180820182526000808252602080830182815285515180855286830151518252600390935543600155845180830193909352518285015283518083038501815260609092019093528051919261045592600292909101906104dc565b505050565b6000808055600181905561047090600290610560565b50565b6040518060400160405280600081526020016104a060408051808201909152600060208201908152815290565b905290565b60408051606081018252600091810191825290819081526020016104a0604051806040016040528060008152602001600081525090565b8280546104e8906106f8565b90600052602060002090601f01602090048101928261050a5760008555610550565b82601f1061052357805160ff1916838001178555610550565b82800160010185558215610550579182015b82811115610550578251825591602001919060010190610535565b5061055c929150610596565b5090565b50805461056c906106f8565b6000825580601f1061057c575050565b601f01602090049060005260206000209081019061047091905b5b8082111561055c5760008155600101610597565b6000604082840312156105bd57600080fd5b50919050565b82815260006020604081840152835180604085015260005b818110156105f7578581018301518582016060015282016105db565b81811115610609576000606083870101525b50601f01601f191692909201606001949350505050565b60006020828403121561063257600080fd5b5035919050565b6040805190810167ffffffffffffffff8111828210171561066a57634e487b7160e01b600052604160045260246000fd5b60405290565b6040516020810167ffffffffffffffff8111828210171561066a57634e487b7160e01b600052604160045260246000fd5b600081830360408112156106b457600080fd5b6106bc610639565b833581526020601f19830112156106d257600080fd5b6106da610670565b91506106e4610670565b602094850135815282529283015250919050565b600181811c9082168061070c57607f821691505b6020821081036105bd57634e487b7160e01b600052602260045260246000fd5b60006040828403121561073e57600080fd5b610746610639565b82518152602083015160208201528091505092915050565b6000821982111561077f57634e487b7160e01b600052601160045260246000fd5b50019056fea2646970667358221220f8289dc6af91e85742559bab0dc1f2527a57723e10f37a9cbc7b1c30f0f116e864736f6c634300080d0033`,
  BytecodeLen: 2870,
  Which: `oD`,
  version: 7,
  views: {
    }
  };
export const _stateSourceMap = {
  2: {
    at: './child.rsh:26:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  3: {
    at: './child.rsh:16:27:after expr stmt semicolon',
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
  "D": D,
  "f": f
  };
export const _APIs = {
  f: f
  };
