import { Reach, test } from '@reach-sh/stdlib';
const stdlib = new Reach(process.env, { REACH_NO_WARN: 'Y' });

const startingBalance = stdlib.parseCurrency(100);
const [minter, receiver] = await stdlib.newTestAccounts(2, startingBalance);

const gasLimit = 500000;
if (stdlib.connector != 'ALGO') {minter.setGasLimit(gasLimit) && receiver.setGasLimit(gasLimit)};

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

const logBalance = async (acc, tok, accStr = acc.getAddress()) => {
    const bal = await getBal(acc, tok, accStr);
    const unit = tok ? 'of the NFT' : stdlib.standardUnit;
    console.log(`${accStr} has ${bal} ${unit}.`);

    return bal;
}

await logBalance(minter, null, "Minter");

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

const mintNFT = async (minter, name, symbol, opts = {supply, url, c, f, defaultFrozen, reserve, note}) => {
    console.log(`Creating the NFT`);
    const theNFT = await stdlib.launchToken(minter, name, symbol, opts);
    console.log(theNFT);
    return theNFT.id;
}

const transferNFT = async (minter, receiver, nftId, supply) => {
    const preAmtNFT = await logBalance(minter, nftId, "Minter");

    if (stdlib.connector == 'ALGO' && await receiver.tokenAccept(nftId)) {
        console.log(`Receiver opted-in to NFT`);
    };
    if (stdlib.connector == 'ALGO' && await receiver.tokenAccepted(nftId)) {
        console.log(`Token accepted`);
    };
    await stdlib.transfer(minter, receiver, supply, nftId);
    console.log(`${supply} ${symbol} transferred from ${minter.getAddress()} to ${receiver.getAddress()}`);

    const postAmtNFT = await logBalance(receiver, nftId, "Receiver");
    test.chk('NFT AMT', preAmtNFT, postAmtNFT);
}

const nftId = await mintNFT(minter, name, symbol, opts);
await transferNFT(minter, receiver, nftId, opts.supply);

await logBalance(minter, null, "Minter");
