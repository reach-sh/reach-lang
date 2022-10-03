import { Reach, test } from '@reach-sh/stdlib';
const stdlib = new Reach(process.env, { REACH_NO_WARN: 'Y' });

const startingBalance = stdlib.parseCurrency(100);

const [minter, receiver] = await stdlib.newTestAccounts(2, startingBalance);

const gasLimit = 500000;
if (stdlib.connector == 'ETH') {minter.setGasLimit(gasLimit)};
if (stdlib.connector == 'ETH') {receiver.setGasLimit(gasLimit)};

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

const mintNFT = async () => {
    console.log(`Creating the NFT`);
    const theNFT = await stdlib.launchToken(minter, "JPalgos", "JPA", { 
        supply: 1, 
        au: "ipfs://placeholderQmYgEFqRkWvNZu7gfk9HDdh55bQbYyc16TF1zX58", //asset url
        c: null, // clawback
        f: null, // freeze address
        defaultFrozen: false, 
        reserve: null, 
        note: Uint8Array[1],
    });
    const nftId = await theNFT.id;
    test.chk('NFT ID', nftId, theNFT.id);

    await transferNFT(nftId);
    return await nftId;
}

const transferNFT = async (nftId) => {

    const preAmtNFT = await minter.balanceOf(nftId);
    console.log(`Minter has ${preAmtNFT} of the NFT`);

    if (stdlib.connector == 'ALGO' && await receiver.tokenAccept(nftId)) {console.log(`Receiver opted-in to NFT`)};
    if (stdlib.connector == 'ALGO' && await receiver.tokenAccepted(nftId)) {console.log(`Token accepted`)};
    await stdlib.transfer(minter, receiver, 1, nftId);
    console.log(`NFT transfer made from minter to receiver`);

    const postAmtNFT = await receiver.balanceOf(nftId);
    console.log(`Receiver has ${postAmtNFT} of the NFT`);
    test.chk('NFT AMT', preAmtNFT, postAmtNFT);
}

await mintNFT();

const postBal = await getBal(minter);
console.log(`Minter balance after transfer: ${postBal}.`);