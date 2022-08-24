import * as fs from 'fs';
import { loadStdlib } from "@reach-sh/stdlib";
const stdlib = loadStdlib(process.env);
const ethers = stdlib.ethers;

if (stdlib.connector !== "ETH") {
    console.log("This test only applies to ETH");
    process.exit(0);
}

const acc = await stdlib.newTestAccount(stdlib.parseCurrency(100));

const lock = () => {
    let unlock;
    const promise = new Promise(r => { unlock = r; });
    return { unlock, promise };
};

const ethersDeploy = async (solOutputPath, ctcName) => {
    const solOutput = fs.readFileSync(solOutputPath);
    const ctcJson = JSON.parse(solOutput)["contracts"][ctcName];
    const factory = new ethers.ContractFactory(ctcJson["abi"], ctcJson["bin"], acc.networkAccount);
    return (await factory.deploy());
}

// Launch OpenZeppelin based ERC721
const oz_erc721 = await ethersDeploy("build/oz_erc721.json", "oz_erc721.sol:OZ_ERC721");

// Launch OpenZeppelin based ERC721TokenReceiver
const oz_erc721tr = await ethersDeploy("build/oz_erc721_tokenreceiver.json",
                                       "oz_erc721_tokenreceiver.sol:OZ_ERC721_TokenReceiver");

// Setup event listener for GotAToken
let oz_recvTokLock = lock();
oz_erc721tr.on("GotAToken", (operator, from, tokenId, data) => {
    console.log(`GotAToken(${operator}, ${from}, ${tokenId}, ${data}`);
    stdlib.assert(tokenId.eq(0x1234));
    stdlib.assert(data == "0x1234");
    oz_recvTokLock.unlock();
    oz_recvTokLock = lock();
});

// OpenZeppelin to OpenZeppelin
await oz_erc721.mint(oz_erc721tr.address, 0x1234, "0x1234")
await oz_recvTokLock.promise;
process.exit(0);
