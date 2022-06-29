// Automatically generated with Reach 0.1.11
/* eslint-disable */
export const _version = '0.1.11';
export const _versionHash = '0.1.11';
export const _backendVersion = 17;

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
  appApproval: ``,
  appClear: ``,
  companionInfo: null,
  extraPages: -1,
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
  Bytecode: `0x608060405260405162000b2238038062000b2283398101604081905262000026916200028e565b6000805543600355604080513381528251602080830191909152830151518183015290517f28822ae872174fb8917549901c639f920e5c2ef0fb881ea78a94dee578586e9d9181900360600190a16200008234156007620000b7565b6200008c62000171565b602080830151518251528181018051600090525143910152620000af81620000e1565b50506200036d565b81620000dd5760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b60208101515160021115620001565760408051808201825260008082526020808301828152855151808552868301515182526003909355436001558451808301939093525182850152835180830385018152606090920190935280519192620001519260029290910190620001ae565b505050565b600080805560018190556200016e906002906200023d565b50565b6040805160608101825260009181019182529081908152602001620001a9604051806040016040528060008152602001600081525090565b905290565b828054620001bc9062000330565b90600052602060002090601f016020900481019282620001e057600085556200022b565b82601f10620001fb57805160ff19168380011785556200022b565b828001600101855582156200022b579182015b828111156200022b5782518255916020019190600101906200020e565b506200023992915062000277565b5090565b5080546200024b9062000330565b6000825580601f106200025c575050565b601f0160209004906000526020600020908101906200016e91905b5b8082111562000239576000815560010162000278565b6000818303604080821215620002a357600080fd5b80518082016001600160401b038082118383101715620002d357634e487b7160e01b600052604160045260246000fd5b818452865183526020601f1986011215620002ed57600080fd5b8351945060208501915084821081831117156200031a57634e487b7160e01b600052604160045260246000fd5b5090915260209384015182529283015250919050565b600181811c908216806200034557607f821691505b602082108114156200036757634e487b7160e01b600052602260045260246000fd5b50919050565b6107a5806200037d6000396000f3fe60806040526004361061004b5760003560e01c80631e93b0f114610054578063763740c414610078578063832307571461008b578063ab53f2c6146100a0578063b3de648b146100c357005b3661005257005b005b34801561006057600080fd5b506003545b6040519081526020015b60405180910390f35b610052610086366004610595565b6100d6565b34801561009757600080fd5b50600154610065565b3480156100ac57600080fd5b506100b5610101565b60405161006f9291906105ad565b6100656100d136600461060a565b61019e565b6040805160208101909152600081526100fd6100f73684900384018461068b565b826101d1565b5050565b600060606000546002808054610116906106e2565b80601f0160208091040260200160405190810160405280929190818152602001828054610142906106e2565b801561018f5780601f106101645761010080835404028352916020019161018f565b820191906000526020600020905b81548152906001019060200180831161017257829003601f168201915b50505050509050915091509091565b60006101a861045d565b60208082015151849052604080519182019052600081526101c982826101d1565b519392505050565b6101e160036000541460096103ad565b81516101fc9015806101f557508251600154145b600a6103ad565b60008080556002805461020e906106e2565b80601f016020809104026020016040519081016040528092919081815260200182805461023a906106e2565b80156102875780601f1061025c57610100808354040283529160200191610287565b820191906000526020600020905b81548152906001019060200180831161026a57829003601f168201915b505050505080602001905181019061029f9190610717565b60408051338152855160208083019190915286015151518183015290519192507face91d8311c1d42194ceea7a64ba4f6f1d530122abc027407da2595ed96d68e1919081900360600190a16102f6341560086103ad565b60208084015151518251918301517fb1885c0fe128efbc823b7323770c75a9e7e8e72714fc480be1ce86ddb4b4304f9261032f91610749565b6103399190610749565b60405190815260200160405180910390a16020808401515151825191830151909161036391610749565b61036d9190610749565b825261037761048f565b8151815152602082015161038d90600190610749565b60208083018051929092529051439101526103a7816103d2565b50505050565b816100fd5760405163100960cb60e01b81526004810182905260240160405180910390fd5b60208101515160021115610444576040805180820182526000808252602080830182815285515180855286830151518252600390935543600155845180830193909352518285015283518083038501815260609092019093528051919261043f92600292909101906104c6565b505050565b6000808055600181905561045a9060029061054a565b50565b60405180604001604052806000815260200161048a60408051808201909152600060208201908152815290565b905290565b604080516060810182526000918101918252908190815260200161048a604051806040016040528060008152602001600081525090565b8280546104d2906106e2565b90600052602060002090601f0160209004810192826104f4576000855561053a565b82601f1061050d57805160ff191683800117855561053a565b8280016001018555821561053a579182015b8281111561053a57825182559160200191906001019061051f565b50610546929150610580565b5090565b508054610556906106e2565b6000825580601f10610566575050565b601f01602090049060005260206000209081019061045a91905b5b808211156105465760008155600101610581565b6000604082840312156105a757600080fd5b50919050565b82815260006020604081840152835180604085015260005b818110156105e1578581018301518582016060015282016105c5565b818111156105f3576000606083870101525b50601f01601f191692909201606001949350505050565b60006020828403121561061c57600080fd5b5035919050565b6040805190810167ffffffffffffffff8111828210171561065457634e487b7160e01b600052604160045260246000fd5b60405290565b6040516020810167ffffffffffffffff8111828210171561065457634e487b7160e01b600052604160045260246000fd5b6000818303604081121561069e57600080fd5b6106a6610623565b833581526020601f19830112156106bc57600080fd5b6106c461065a565b91506106ce61065a565b602094850135815282529283015250919050565b600181811c908216806106f657607f821691505b602082108114156105a757634e487b7160e01b600052602260045260246000fd5b60006040828403121561072957600080fd5b610731610623565b82518152602083015160208201528091505092915050565b6000821982111561076a57634e487b7160e01b600052601160045260246000fd5b50019056fea2646970667358221220cc2664a7289f805e93dd43ec784c2a7f6b0c5135c766170fdc719998a7fffe6264736f6c634300080c0033`,
  BytecodeLen: 2850,
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
