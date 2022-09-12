import { loadStdlib } from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(1000);

const accMinter = await stdlib.newTestAccount(startingBalance);
const receiver = await stdlib.newTestAccount(startingBalance);

const gasLimit = 5000000;
accMinter.setGasLimit(gasLimit);
receiver.setGasLimit(gasLimit);

const mintAddr = accMinter.getAddress();
console.log(`Your address is ${mintAddr}`);
const recAddr = receiver.getAddress();
console.log(`receiver's address is ${recAddr}`);

const minterAddrFormat = await stdlib.formatAddress(accMinter);
console.log(`The minter's formatted address is ${minterAddrFormat}`);
const receiverAddrFormat = await stdlib.formatAddress(receiver);
console.log(`The receiver's formatted address is ${receiverAddrFormat}`);

const fmt = (x) => stdlib.formatCurrency(x, 4);
const getBal = async (who) => fmt(await stdlib.balanceOf(who));
const bal = await getBal(accMinter);
console.log(`Minter starting balance: ${bal}.`);

console.log(`Creating the NFT`);
const theNFT = await stdlib.launchToken(accMinter, "nftName", "SYM", { supply: 1});
const nftId = theNFT.id;
    
await receiver.tokenAccept(nftId);
if (stdlib.connector == 'ALGO') {console.log(`Receiver opted-in to NFT`)};
receiver.tokenAccepted(nftId);
if (stdlib.connector == 'ALGO') {console.log(`Token accepted`)};
await stdlib.transfer(accMinter, receiver, 1, nftId);
console.log(`NFT transfer made from minter to receiver`);

console.log(`Minter balance after transfer: ${bal}.`);