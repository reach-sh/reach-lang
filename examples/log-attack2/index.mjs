  import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
  import * as backend from './build/index.main.mjs';
  import real_ethers from 'ethers';
  import * as cfxers from '@reach-sh/stdlib/cfxers.mjs';
  import * as fs from 'fs';
  
  
  (async () => {
    const stdlib = await stdlib_loader.loadStdlib();
    const ethers = stdlib.connector === 'CFX' ? cfxers : real_ethers;
    const startingBalance = stdlib.parseCurrency(10);
    const accAlice = await stdlib.newTestAccount(startingBalance);
    const myGasLimit = 8000000;
    accAlice.setGasLimit(myGasLimit);

    
    const compiled = JSON.parse(await fs.readFileSync('./build/index.sol.json'));
    console.log(`Alice read compiled file: ${JSON.stringify(compiled)}`);
    const remoteCtc = compiled["contracts"]["index.sol:LogAttack2"];
    const remoteABI = remoteCtc["abi"];
    const remoteBytecode = remoteCtc["bin"];
    const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accAlice.networkAccount);
    const contract = await factory.deploy({ gasLimit: myGasLimit });
    await contract.deployed();
    console.log(`SOLIDITY CONTRACT ADDR : ${contract.address}`);
    console.log(`Tx Hash: ${contract.deployTransaction.hash}`);
    const solidity = contract.address;
    const logAttack2 = new ethers.Contract(solidity, remoteABI, accAlice.networkAccount);

    const  time = '08102021';
    const amt = '101010101'
    const ctcAlice = accAlice.deploy(backend);
    const localAddr = await ctcAlice.getInfo();
    const addrs = await accAlice.getAddress();
    
    const bid = await logAttack2.m2(addrs, amt);
    console.log(bid);
    const date = await logAttack2.m3(time);
    console.log(date);

    const event2  = logAttack2.filters.e2();
    console.log(event2);
    const event3  = logAttack2.filters.e3();
    console.log(event3);
    logAttack2.on(event2, console.log);
    logAttack2.on(event3, console.log);


    await Promise.all([
      backend.Alice(ctcAlice, {
        ...stdlib.hasConsoleLogger,
        m2: (() => [ logAttack2, amt ]),
        m3: (() =>  time ),
     }),
  ]);
      console.log(`REACH ADDR : ${localAddr}`)  
      console.log(`Alice address: ${addrs}`);
})();