// Automatically generated with Reach 0.1.0

export async function Alice(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  
  
  const txn1 = await ctc.sendrecv('Alice', 1, 1, [stdlib.protect(stdlib.T_UInt256, interact.request)], 0, false, null);
  const [v1] = txn1.data;
  const v2 = txn1.from;
  const v3 = txn1.value;
  const v5 = stdlib.eq(0, v3);
  stdlib.assert(v5);
  const txn2 = await ctc.recv('Alice', 2, 0, false);
  const [] = txn2.data;
  const v8 = txn2.from;
  const v9 = txn2.value;
  const v11 = stdlib.eq(v1, v9);
  stdlib.assert(v11);
  
  
  const txn3 = await ctc.sendrecv('Alice', 3, 1, [v2, v1, stdlib.protect(stdlib.T_Bytes, interact.info)], 0, false, null);
  const [v13] = txn3.data;
  const v14 = txn3.value;
  const v16 = stdlib.eq(0, v14);
  stdlib.assert(v16);
  return; }
export async function Bob(stdlib, ctc, interact) {
  const txn0 = { balance: 0, value: 0 };
  const txn1 = await ctc.recv('Bob', 1, 1, false);
  const [v1] = txn1.data;
  const v2 = txn1.from;
  const v3 = txn1.value;
  const v5 = stdlib.eq(0, v3);
  stdlib.assert(v5);
  stdlib.protect(stdlib.T_Null, await interact.want(v1));
  
  
  const txn2 = await ctc.sendrecv('Bob', 2, 0, [v2, v1], v1, false, null);
  const [] = txn2.data;
  const v8 = txn2.from;
  const v9 = txn2.value;
  const v11 = stdlib.eq(v1, v9);
  stdlib.assert(v11);
  const txn3 = await ctc.recv('Bob', 3, 1, false);
  const [v13] = txn3.data;
  const v14 = txn3.value;
  const v16 = stdlib.eq(0, v14);
  stdlib.assert(v16);
  stdlib.protect(stdlib.T_Null, await interact.got(v13));
  
  return; }

const _ETH = {
  ABI: `[
    {
      "inputs": [],
      "stateMutability": "payable",
      "type": "constructor"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "v1",
          "type": "uint256"
        }
      ],
      "name": "e1",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        }
      ],
      "name": "e2",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "_bal",
          "type": "uint256"
        },
        {
          "indexed": false,
          "internalType": "bytes",
          "name": "v13",
          "type": "bytes"
        }
      ],
      "name": "e3",
      "type": "event"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "v1",
          "type": "uint256"
        }
      ],
      "name": "m1",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v2",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v1",
          "type": "uint256"
        }
      ],
      "name": "m2",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "uint256",
          "name": "_last",
          "type": "uint256"
        },
        {
          "internalType": "address payable",
          "name": "v2",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "v1",
          "type": "uint256"
        },
        {
          "internalType": "bytes",
          "name": "v13",
          "type": "bytes"
        }
      ],
      "name": "m3",
      "outputs": [],
      "stateMutability": "payable",
      "type": "function"
    }
  ]`,
  Bytecode: `0x608060405261001160004360a0610031565b60408051601f19818403018152919052805160209091012060005561003f565b918252602082015260400190565b6104618061004e6000396000f3fe6080604052600436106100345760003560e01c80631471648214610039578063502071211461004e578063cec9742c14610061575b600080fd5b61004c6100473660046102b9565b610074565b005b61004c61005c366004610381565b61012d565b61004c61006f3660046102f0565b6101e2565b600183838360405160200161008c94939291906103ef565b6040516020818303038152906040528051906020012060001c600054146100b257600080fd5b3481146100be57600080fd5b7ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e9476040516100ed91906103a2565b60405180910390a1600243838360405160200161010d94939291906103ef565b60408051601f198184030181529190528051602090910120600055505050565b6000826040516020016101419291906103e1565b6040516020818303038152906040528051906020012060001c6000541461016757600080fd5b341561017257600080fd5b7fc9d006980a027d32b08195d9eab835f93f490ac52038c85ed3e8aca6e0b9b2e147826040516101a39291906103e1565b60405180910390a160014333836040516020016101c394939291906103ef565b60408051601f1981840301815291905280516020909101206000555050565b60028585856040516020016101fa94939291906103ef565b6040516020818303038152906040528051906020012060001c6000541461022057600080fd5b336001600160a01b0385161461023557600080fd5b341561024057600080fd5b6040516001600160a01b0385169084156108fc029085906000818181858888f19350505050158015610276573d6000803e3d6000fd5b507fb81676930764008488d8dff03d723d3671d6123701e07a915c616720379a6a6b4783836040516102aa939291906103ab565b60405180910390a16000805533ff5b6000806000606084860312156102cd578283fd5b8335925060208401356102df81610413565b929592945050506040919091013590565b600080600080600060808688031215610307578081fd5b85359450602086013561031981610413565b935060408601359250606086013567ffffffffffffffff8082111561033c578283fd5b818801915088601f83011261034f578283fd5b81358181111561035d578384fd5b89602082850101111561036e578384fd5b9699959850939650602001949392505050565b60008060408385031215610393578182fd5b50508035926020909101359150565b90815260200190565b60008482526040602083015282604083015282846060840137818301606090810191909152601f909201601f1916010192915050565b918252602082015260400190565b93845260208401929092526001600160a01b03166040830152606082015260800190565b6001600160a01b038116811461042857600080fd5b5056fea26469706673582212206f63f3f92de13acffcd3476afaf841208b8ae715e3dcc7a6ba5270036cf7d2bb64736f6c63430007000033` };

export const _Connectors = {
  ETH: _ETH };
