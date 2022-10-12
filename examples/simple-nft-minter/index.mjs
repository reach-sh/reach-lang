import { Reach, test } from '@reach-sh/stdlib';
const stdlib = new Reach(process.env, { REACH_NO_WARN: 'Y' });

const startingBalance = stdlib.parseCurrency(100);

const [minter, receiver] = await stdlib.newTestAccounts(2, startingBalance);

const gasLimit = 500000;
if (stdlib.connector != 'ALGO') {minter.setGasLimit(gasLimit)};
if (stdlib.connector != 'ALGO') {receiver.setGasLimit(gasLimit)};

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

const name = "JPAlgos";
const symbol = "JPA";

const opts = { 
    supply: 1, 
    au: "ipfs://bafybeigdyrzt5...", //asset url
    c: null, // clawback
    f: null, // freeze address
    defaultFrozen: false, 
    reserve: null, 
    note: Uint8Array[1],
};

const mintNFT = async (minter, name, symbol, opts = {supply: 1, au: "ipfs://bafybeigdyrzt5...", c: null, f: null, defaultFrozen: false, reserve: null, note: Uint8Array[1]}) => {
    console.log(`Creating the NFT`);
    const theNFT = await stdlib.launchToken(minter, name, symbol, opts);
    console.log(theNFT);
    return theNFT.id;
}

const transferNFT = async (minter, receiver, nftId, supply) => {

    const preAmtNFT = await minter.balanceOf(nftId);
    console.log(`Minter has ${preAmtNFT} of the NFT`);

    if (stdlib.connector == 'ALGO' && await receiver.tokenAccept(nftId)) {
        console.log(`Receiver opted-in to NFT`);
    };
    if (stdlib.connector == 'ALGO' && await receiver.tokenAccepted(nftId)) {
        console.log(`Token accepted`);
    };
    await stdlib.transfer(minter, receiver, supply, nftId);
    console.log(`NFT transfer made from minter to receiver`);

    const postAmtNFT = await receiver.balanceOf(nftId);
    console.log(`Receiver has ${postAmtNFT} of the NFT`);
    test.chk('NFT AMT', preAmtNFT, postAmtNFT);
}

const nftId = await mintNFT(minter, name, symbol, opts);
await transferNFT(minter, receiver, nftId, opts.supply);

const postBal = await getBal(minter);
console.log(`Minter balance after transfer: ${postBal}.`);