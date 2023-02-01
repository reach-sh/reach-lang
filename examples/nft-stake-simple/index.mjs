import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const fmt = (x) => stdlib.formatCurrency(x, 4);
const getBal = async (who) => fmt(await stdlib.balanceOf(who));

const [ accA, accB ] =
  await stdlib.newTestAccounts(2, startingBalance);
const beforeA = await getBal(accA);
const beforeB = await getBal(accB);
console.log('Hello, Clarice...');

console.log('Launching...');
const ctcA = accA.contract(backend);
const ctcB = accB.contract(backend, ctcA.getInfo());

const NFT = await stdlib.launchToken(accA, "Name", "SYM", {supply: 1});
await accB.tokenAccept(NFT.id);
const beforeBal = await stdlib.balanceOf(accA, NFT.id);
console.log(`Admin has ${beforeBal} of the NFT`);

await stdlib.transfer(accA, accB, 1, NFT.id);// simulate third-party token sale
console.log(`Third party sale of the NFT is complete`);
const afterSale = await stdlib.balanceOf(accB, NFT.id);
console.log(`User has ${afterSale} of the NFT`);

console.log('Starting backends...');
await Promise.all([
  backend.Admin(ctcA, {
    params: {
      tok: NFT.id,
      rewards: stdlib.parseCurrency(10),
      deadline: 25,// in blocks
    },
    launched: async (c) => {
      console.log(`Ready at contract ${c}`);
      // trigger the view and show to Bob
      const [tok, rewards, deadline] = await ctcB.unsafeViews.seeTerms();
      console.log(`The frontend sees the terms are: \nNFT: ${tok}\n Rewards: ${stdlib.formatCurrency(rewards)} ${stdlib.standardUnit}\n Length: ${deadline} blocks`);
    },
    checkStatus: async () => {
      const contractAddr = await ctcA.getContractAddress();
      const nftBalContract = await stdlib.balanceOf(contractAddr, NFT.id);
      console.log(`The Contract holds ${nftBalContract} of the NFT`);
      return true;
    },
  }),
  backend.User(ctcB, {}),
]);

const afterA = await getBal(accA);
const afterB = await getBal(accB);

console.log(`Admin went from ${beforeA} to ${afterA}`);
console.log(`User went from ${beforeB} to ${afterB}`);

const contractAddr = await ctcA.getContractAddress();
const nftBalC = await stdlib.balanceOf(contractAddr, NFT.id);
console.log(`Contract NFT balance: ${nftBalC}`);

const nftBalA = await accA.balanceOf(NFT.id);
const nftBalB = await accB.balanceOf(NFT.id);

console.log(`Admin NFT balance: ${nftBalA}`);
console.log(`User NFT balance: ${nftBalB}`);

console.log('Exiting...');
