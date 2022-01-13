import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from "./build/mv.main.mjs";
import dotenv from 'dotenv';

dotenv.config();
(async()=>{
    
    const reachStdlib =  loadStdlib(process.env);
    const creatorAccount, ownerAccount = await reachStdlib.newTestAccount(reachStdlib.parseCurrency(100));
    const ctc =  creatorAccount.contract(backend);
    const contract =  ownerAccount.contract(backend, 40);
    const view = contract.v.NFT;

    ctc.getInfo().then(info =>{
        console.log(info);
    })
    console.log("Program started")
    await backend.Creator(ctc, {getId:() => { return 2}})
    try{
      const contractView = await view.owner();
      if (contractView[0] === 'None') {
        throw new Error('the view returned none');
      }
      const viewObj = contractView[1];
      const update = contract.a.Owner;
      const returnValue = await update.newOwner("GWV4ZZTXZQTJMBNFIFOPE6ETHNZM2UQKSI6MLY3HK4BH4EYJW6CFIGMM4M")
      console.log(viewObj);
    }catch(e){
      console.log(e);
    }
    console.log("Program ended")
})()