import { Reach, test } from '@reach-sh/stdlib';
const stdlib = new Reach(process.env, { REACH_NO_WARN: 'Y' });

const startingBalance = stdlib.parseCurrency(100);
const [minter, receiver] = await stdlib.newTestAccounts(2, startingBalance);
minter.setDebugLabel('Minter');
receiver.setDebugLabel('Receiver');

const gasLimit = 500000;
if (stdlib.connector != 'ALGO') {
    minter.setGasLimit(gasLimit) && receiver.setGasLimit(gasLimit)
};

const mintAddr = minter.getAddress();
console.log(`Minter's address is ${mintAddr}`);
const recAddr = receiver.getAddress();
console.log(`Receiver's address is ${recAddr}`);

const minterAddrFormat = await stdlib.formatAddress(minter);
console.log(`The minter's formatted address is ${minterAddrFormat}`);
const receiverAddrFormat = await stdlib.formatAddress(receiver);
console.log(`The receiver's formatted address is ${receiverAddrFormat}`);

const fmt = (x) => stdlib.formatCurrency(x, 4);
const getBal = async (who, tok) => tok ? (await stdlib.balanceOf(who, tok)) : fmt(await stdlib.balanceOf(who));

const logBalance = async (acc, tok) => {
    const bal = await getBal(acc, tok);
    const unit = tok ? 'of the NFT' : stdlib.standardUnit;
    console.log(`${acc.getDebugLabel()} has ${bal} ${unit}.`);
    return bal;
}

await logBalance(minter);

const name = (stdlib.connector == 'ALGO') ? "JPAlgos" : "JPals";
const symbol = (stdlib.connector == 'ALGO') ? "JPA" : "JPAL";

const opts = { 
    supply: 1, 
    url: "ipfs://bafybeigdyrzt5...", //asset url
    c: null, // clawback
    f: null, // freeze address
    defaultFrozen: false, 
    reserve: null, 
    note: Uint8Array[1],
};

const mintNFT = async (minter, name, symbol, opts) => {
    console.log(`Creating the NFT`);
    const theNFT = await stdlib.launchToken(minter, name, symbol, opts);
    console.log(theNFT);
    return theNFT.id;
}

const transferNFT = async (minter, receiver, nftId, supply) => {
    const preAmtNFT = await logBalance(minter, nftId);

    if (stdlib.connector == 'ALGO') {
        await receiver.tokenAccept(nftId);
        console.log(`${receiver.getDebugLabel()} opted-in to NFT`);
    };
    await stdlib.transfer(minter, receiver, supply, nftId);
    console.log(`${supply} ${symbol} transferred from ${minter.getDebugLabel()} to ${receiver.getDebugLabel()}`);
    
    const postAmtNFT = await logBalance(receiver, nftId);
    test.chk('NFT AMT', preAmtNFT, postAmtNFT);
}

const nftId = await mintNFT(minter, name, symbol, opts);
await transferNFT(minter, receiver, nftId, opts.supply);
await logBalance(minter);
