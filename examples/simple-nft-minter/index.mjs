import { Reach, test } from '@reach-sh/stdlib';
const stdlib = new Reach(process.env, { REACH_NO_WARN: 'Y' });

const startingBalance = stdlib.parseCurrency(100);
const [minter, receiver] = await stdlib.newTestAccounts(2, startingBalance);

const gasLimit = 500000;
if (stdlib.connector != 'ALGO') {minter.setGasLimit(gasLimit)};
if (stdlib.connector != 'ALGO') {receiver.setGasLimit(gasLimit)};

const mintAddr = minter.getAddress();
console.log(`Minter's address is ${mintAddr}`);
const recAddr = receiver.getAddress();
console.log(`Receiver's address is ${recAddr}`);

const minterAddrFormat = await stdlib.formatAddress(minter);
console.log(`The minter's formatted address is ${minterAddrFormat}`);
const receiverAddrFormat = await stdlib.formatAddress(receiver);
console.log(`The receiver's formatted address is ${receiverAddrFormat}`);

const fmt = (x) => stdlib.formatCurrency(x, 4);
const getBal = async (who, tok) => fmt(await stdlib.balanceOf(who, tok));

const logBalance = async (acc, tok, accStr) => {
    const bal = await getBal(acc, tok, accStr);
    const trueBal = await tok ? opts.supply : bal;
    const unit = await tok ? 'of the NFT' : stdlib.connector;
    accStr = acc.networkAccount.addr.toString(); 
    console.log(`${accStr} has ${trueBal} ${unit}.`);

    return trueBal;
}

await logBalance(minter);

const name = "JPAlgos";
const symbol = "JPA";

const opts = { 
    supply: 1, 
    url: "ipfs://bafybeigdyrzt5...", //asset url
    c: null, // clawback
    f: null, // freeze address
    defaultFrozen: false, 
    reserve: null, 
    note: Uint8Array[1],
};

const mintNFT = async (minter, name, symbol, opts = {supply, url, c: null, f: null, defaultFrozen: false, reserve: null, note}) => {
    console.log(`Creating the NFT`);
    const theNFT = await stdlib.launchToken(minter, name, symbol, opts);
    console.log(theNFT);
    return theNFT.id;
}

const transferNFT = async (minter, receiver, nftId, supply) => {
    const preAmtNFT = await logBalance(minter, nftId);

    if (stdlib.connector == 'ALGO' && await receiver.tokenAccept(nftId)) {
        console.log(`Receiver opted-in to NFT`);
    };
    if (stdlib.connector == 'ALGO' && await receiver.tokenAccepted(nftId)) {
        console.log(`Token accepted`);
    };
    await stdlib.transfer(minter, receiver, supply, nftId);
    console.log(`${supply} ${symbol} transferred from ${minter.networkAccount.addr.toString()} to ${receiver.networkAccount.addr.toString()}`);

    const postAmtNFT = await logBalance(receiver, nftId);
    test.chk('NFT AMT', preAmtNFT, postAmtNFT);
}

const nftId = await mintNFT(minter, name, symbol, opts);
await transferNFT(minter, receiver, nftId, opts.supply);

await logBalance(minter);
