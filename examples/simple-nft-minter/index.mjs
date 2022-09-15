import { loadStdlib } from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100000);

const [minter, receiver] = await stdlib.newTestAccounts(2, startingBalance);

const gasLimit = 5000000;
minter.setGasLimit(gasLimit);
receiver.setGasLimit(gasLimit);

const mintAddr = minter.getAddress();
console.log(`Your address is ${mintAddr}`);
const recAddr = receiver.getAddress();
console.log(`receiver's address is ${recAddr}`);

const minterAddrFormat = await stdlib.formatAddress(minter);
console.log(`The minter's formatted address is ${minterAddrFormat}`);
const receiverAddrFormat = await stdlib.formatAddress(receiver);
console.log(`The receiver's formatted address is ${receiverAddrFormat}`);

const fmt = (x) => stdlib.formatCurrency(x, 4);
const getBal = async (who) => fmt(await stdlib.balanceOf(who));
const bal = await getBal(minter);
console.log(`Minter starting balance: ${bal}.`);

console.log(`Creating the NFT`);
const theNFT = await stdlib.launchToken(minter, "nftName", "SYM", { supply: 1, url: "https://reach.sh", clawback: null, freeze: null, defaultFrozen: false, reserve: null, note: Uint8Array[1]});
const nftId = theNFT.id;

const [preAmtNFT] = await stdlib.balancesOf(minter, [nftId]);
console.log(`Minter has ${preAmtNFT} of the NFT`);
    
if (stdlib.connector == 'ALGO' && await receiver.tokenAccept(nftId)) {console.log(`Receiver opted-in to NFT`)};
if (stdlib.connector == 'ALGO' && await receiver.tokenAccepted(nftId)) {console.log(`Token accepted`)};
await stdlib.transfer(minter, receiver, 1, nftId);
console.log(`NFT transfer made from minter to receiver`);

const postBal = await getBal(minter);
console.log(`Minter balance after transfer: ${postBal}.`);

const [postAmtNFT] = await stdlib.balancesOf(receiver, [nftId]);
console.log(`Receiver has ${postAmtNFT} of the NFT`);