import { loadStdlib } from "@reach-sh/stdlib";
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const funds = stdlib.parseCurrency(100);
const price = stdlib.parseCurrency(10);

const su = stdlib.standardUnit;

console.log(`You are connected to ${stdlib.connector}.`);

const accMaker = await stdlib.newTestAccount(funds);
const accBuyer = await stdlib.newTestAccount(funds);

const myGasLimit = 5_000_000;
accMaker.setGasLimit(myGasLimit);
accBuyer.setGasLimit(myGasLimit);

const fmt = (x) => stdlib.formatCurrency(x, 4);

const ctcMaker = accMaker.contract(backend);
const ctcBuyer = accBuyer.contract(backend, ctcMaker.getInfo());

const makerBalance = fmt(await stdlib.balanceOf(accMaker));
const buyerBalance = fmt(await stdlib.balanceOf(accBuyer));

console.log('New accounts created.');


const amt = 1
const firstNFT = await stdlib.launchToken(accMaker, "nft1", "FRST", {supply: 1 });

await accMaker.tokenAccept(firstNFT.id);
await accBuyer.tokenAccept(firstNFT.id);

const amtNFTmaker = await stdlib.balancesOf(accMaker, [ firstNFT.id ] );
const amtNFTbuyer = await stdlib.balancesOf(accBuyer, [ firstNFT.id ] );
console.log(`Maker has ${makerBalance} ${su}.`);
console.log(`Buyer has ${buyerBalance} ${su}.`);

await Promise.all ([
  backend.Maker(
    ctcMaker,
    { 
	  nftID: () => {
        console.log(`Maker makes a ${firstNFT.sym} NFT with an ID of ${firstNFT.id}.`);
	    console.log(`The ${firstNFT.sym} NFT is for sale for ${fmt(price)} ${su}.`);
        console.log(`Maker has ${amtNFTmaker} ${firstNFT.sym}.`);
        console.log(`Buyer has ${amtNFTbuyer} ${firstNFT.sym}.`);
        return firstNFT.id;
	},
	  firstNFT: firstNFT.id,
	  price,
	  amt
  }),
  backend.Buyer(
    ctcBuyer, 
  {
      purchase: async (price) =>
        console.log(`${stdlib.formatAddress(accBuyer)} purchased ${firstNFT.id} for ${fmt(price)} ${su}.`)
  }),
]);

const newMakerBalance = fmt(await stdlib.balanceOf(accMaker));
const newBuyerBalance = fmt(await stdlib.balanceOf(accBuyer));
const newamtNFTmaker = await stdlib.balancesOf(accMaker, [ firstNFT.id ] );
const newamtNFTbuyer = await stdlib.balancesOf(accBuyer, [ firstNFT.id ] );

console.log(`Maker has ${newMakerBalance} ${su}.`);
console.log(`Buyer has ${newBuyerBalance} ${su}.`);

console.log(`Maker has ${newamtNFTmaker} ${firstNFT.sym}.`);
console.log(`Buyer has ${newamtNFTbuyer} ${firstNFT.sym}.`);
