// Automatically generated with Reach 0.1.12 (c4d5d416*)
/* eslint-disable */
export const _version = '0.1.12';
export const _versionHash = '0.1.12 (c4d5d416*)';
export const _backendVersion = 25;

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
    lct: stdlib.checkedBigNumberify('./index.rsh:17:5:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./index.rsh:17:5:decimal', stdlib.UInt_max, '0'), []],
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
    pay: [stdlib.checkedBigNumberify('./index.rsh:27:5:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v82], secs: v84, time: v83, didSend: v46, from: v81 } = txn3;
      
      ;
      sim_r.txns.push({
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
    at: './index.rsh:27:5:dot',
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
    at: './index.rsh:21:18:application',
    fs: ['at ./index.rsh:20:9:application call to [unknown function] (defined at: ./index.rsh:20:13:function exp)'],
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
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v82), {
    at: './index.rsh:32:17:application',
    fs: ['at ./index.rsh:31:9:application call to [unknown function] (defined at: ./index.rsh:31:13:function exp)'],
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
  appApproval: `ByAEAAECICYCAQAAIjUAMRhBAVApZEkiWzUBgQhbNQI2GgAXSUEAByI1BCM1BgA2GgIXNQQ2GgM2GgEXSSMMQACRSSQMQABHJBJEJDQBEkQ0BEkiEkw0AhIRRChkSTUDVwAgNf9JNQU1/oAEXpV+NDT+ULA0/zEAEkSxIrIBNAMlW7III7IQNP+yB7NCAIVIIzQBEkQ0BEkiEkw0AhIRRChkSTUDSVcAIDX/JVs1/oAEmouRdLA0/ogAxjT/NP4WUChLAVcAKGdIJDUBMgY1AkIAXUiBoI0GiACmIjQBEkQ0BEkiEkw0AhIRREk1BRc1/4AEgsRh/jT/FlCwMQA0/xZQKEsBVwAoZ0gjNQEyBjUCQgAcMRmBBRJEsSKyASKyCCOyEDIJsgkyCrIHs0IABTEZIhJEKTQBFjQCFlBnNAZBAAqABBUffHU0B1CwNABJIwgyBBJEMRYSRCNDMRkiEkRC/98iMTQSRCQxNRJEIjE2EkQiMTcSRCI1ASI1AkL/rzQASUojCDUAOAcyChJEOBAjEkQ4CBJEiQ==`,
  appClear: `Bw==`,
  companionInfo: null,
  extraPages: 0,
  mapDataKeys: 0,
  mapDataSize: 0,
  stateKeys: 1,
  stateSize: 40,
  unsupported: [],
  version: 11,
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
            "name": "elem0",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "elem1",
            "type": "uint256"
          }
        ],
        "internalType": "struct T0",
        "name": "v129",
        "type": "tuple"
      }
    ],
    "stateMutability": "nonpayable",
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
            "name": "elem0",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "elem1",
            "type": "uint256"
          }
        ],
        "indexed": false,
        "internalType": "struct T0",
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
            "name": "elem0",
            "type": "uint256"
          }
        ],
        "indexed": false,
        "internalType": "struct T2",
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
            "name": "elem0",
            "type": "uint256"
          },
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
            "internalType": "struct T3",
            "name": "elem1",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T4",
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
            "name": "elem0",
            "type": "uint256"
          }
        ],
        "internalType": "struct T2",
        "name": "v131",
        "type": "tuple"
      }
    ],
    "name": "_reachp_1",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "elem0",
            "type": "uint256"
          },
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
            "internalType": "struct T3",
            "name": "elem1",
            "type": "tuple"
          }
        ],
        "internalType": "struct T4",
        "name": "v133",
        "type": "tuple"
      }
    ],
    "name": "_reachp_2",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
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
  Bytecode: `0x60806040523480156200001157600080fd5b5060405162000c1538038062000c1583398101604081905262000034916200016d565b60008080554360035560408051602081019091529081526200005782826200005f565b50506200031a565b6040805133815283516020808301919091528401518183015290517f6de3f97962105ba8e929dd0da178e54f00336c9ea6154699025bad3d4f17547f9181900360600190a18151620000c2901580620000ba57508251600154145b60076200012d565b620000d0341560086200012d565b6040805180820182526000602080830182815233808552878301518252600193849055439093558451918201929092529051818401528251808203840181526060909101909252906002906200012790826200024e565b50505050565b81620001535760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b634e487b7160e01b600052604160045260246000fd5b6000604082840312156200018057600080fd5b604080519081016001600160401b0381118282101715620001a557620001a562000157565b604052825181526020928301519281019290925250919050565b600181811c90821680620001d457607f821691505b602082108103620001f557634e487b7160e01b600052602260045260246000fd5b50919050565b601f8211156200024957600081815260208120601f850160051c81016020861015620002245750805b601f850160051c820191505b81811015620002455782815560010162000230565b5050505b505050565b81516001600160401b038111156200026a576200026a62000157565b62000282816200027b8454620001bf565b84620001fb565b602080601f831160018114620002ba5760008415620002a15750858301515b600019600386901b1c1916600185901b17855562000245565b600085815260208120601f198616915b82811015620002eb57888601518255948401946001909101908401620002ca565b50858210156200030a5787850151600019600388901b60f8161c191681555b5050505050600190811b01905550565b6108eb806200032a6000396000f3fe60806040526004361061004b5760003560e01c80631e93b0f114610054578063573b85101461007857806357a58e181461009b57806383230757146100ae578063ab53f2c6146100c357005b3661005257005b005b34801561006057600080fd5b506003545b6040519081526020015b60405180910390f35b61008b610086366004610585565b6100e6565b604051901515815260200161006f565b61008b6100a9366004610597565b610115565b3480156100ba57600080fd5b50600154610065565b3480156100cf57600080fd5b506100d861013e565b60405161006f9291906105a9565b604080516020810190915260008082529061010f6101093685900385018561063e565b826101db565b50919050565b604080516020810190915260008082529061010f61013836859003850185610680565b82610356565b6000606060005460028080546101539061070f565b80601f016020809104026020016040519081016040528092919081815260200182805461017f9061070f565b80156101cc5780601f106101a1576101008083540402835291602001916101cc565b820191906000526020600020905b8154815290600101906020018083116101af57829003601f168201915b50505050509050915091509091565b60408051338152835160208201527fcf0e8bec53cd91fa87ecf8f6f405ac75914a22acdb92a3553ee5c294fee81596910160405180910390a16102246001600054146009610506565b6000600280546102339061070f565b80601f016020809104026020016040519081016040528092919081815260200182805461025f9061070f565b80156102ac5780601f10610281576101008083540402835291602001916102ac565b820191906000526020600020905b81548152906001019060200180831161028f57829003601f168201915b50505050508060200190518101906102c49190610743565b83519091506102e29015806102db57508351600154145b600a610506565b6102f381602001513414600b610506565b6040805180820182526000808252602080830182815285516001600160a01b03168085528683015182526002938490554360015585519283015251818501528351808203850181526060909101909352909161034f90826107f5565b5050505050565b6040805133815283516020808301919091528085015180518385015290810151606080840191909152818401516080840152015160a082015290517f3380acc111c840aea9ad8d1987b32a3eb2bf2736fb9b69768ab6c1c26ed72b379181900360c00190a16103cb600260005414600c610506565b6000600280546103da9061070f565b80601f01602080910402602001604051908101604052809291908181526020018280546104069061070f565b80156104535780601f1061042857610100808354040283529160200191610453565b820191906000526020600020905b81548152906001019060200180831161043657829003601f168201915b505050505080602001905181019061046b9190610743565b835190915061048990158061048257508351600154145b600d610506565b6104953415600e610506565b80516104ad906001600160a01b03163314600f610506565b805160208201516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156104ea573d6000803e3d6000fd5b50600080805560018190556105019060029061052f565b505050565b8161052b5760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b50805461053b9061070f565b6000825580601f1061054b575050565b601f016020900490600052602060002090810190610569919061056c565b50565b5b80821115610581576000815560010161056d565b5090565b60006020828403121561010f57600080fd5b600060a0828403121561010f57600080fd5b82815260006020604081840152835180604085015260005b818110156105dd578581018301518582016060015282016105c1565b506000606082860101526060601f19601f830116850101925050509392505050565b634e487b7160e01b600052604160045260246000fd5b6040516080810167ffffffffffffffff81118282101715610638576106386105ff565b60405290565b60006020828403121561065057600080fd5b6040516020810181811067ffffffffffffffff82111715610673576106736105ff565b6040529135825250919050565b600081830360a081121561069357600080fd5b6040516040810181811067ffffffffffffffff821117156106b6576106b66105ff565b604052833581526080601f19830112156106cf57600080fd5b6106d7610615565b915060208401358252604084013560208301526060840135604083015260808401356060830152816020820152809250505092915050565b600181811c9082168061072357607f821691505b60208210810361010f57634e487b7160e01b600052602260045260246000fd5b60006040828403121561075557600080fd5b6040516040810181811067ffffffffffffffff82111715610778576107786105ff565b60405282516001600160a01b038116811461079257600080fd5b81526020928301519281019290925250919050565b601f82111561050157600081815260208120601f850160051c810160208610156107ce5750805b601f850160051c820191505b818110156107ed578281556001016107da565b505050505050565b815167ffffffffffffffff81111561080f5761080f6105ff565b6108238161081d845461070f565b846107a7565b602080601f83116001811461085857600084156108405750858301515b600019600386901b1c1916600185901b1785556107ed565b600085815260208120601f198616915b8281101561088757888601518255948401946001909101908401610868565b50858210156108a55787850151600019600388901b60f8161c191681555b5050505050600190811b0190555056fea264697066735822122066b907640ea5e285f259763322f832194c34ff38a9df930a0f2c06dae3f529ab64736f6c63430008100033`,
  BytecodeLen: 3093,
  Which: `oD`,
  version: 8,
  views: {
    }
  };
export const _stateSourceMap = {
  1: {
    at: './index.rsh:18:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  2: {
    at: './index.rsh:23:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  3: {
    at: './index.rsh:29:11:after expr stmt semicolon',
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
