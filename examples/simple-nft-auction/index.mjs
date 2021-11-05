import { loadStdlib } from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

const N = 3;
const names = ["Creator", "Alice", "Bob", "Carla"];

(async () => {
  const stdlib = await loadStdlib(process.env);
  const startingBalance = stdlib.parseCurrency(10);
  const [ accCreator, ...accBidders ] =
    await stdlib.newTestAccounts(1+N, startingBalance);
  // We're including this for automation, but it would be better if the NFT is
  // assumed to already exist, or if it this contract actually created it.
  const theNFT = await stdlib.launchToken(accCreator, "beepboop", "NFT", { supply: 1 });

  await Promise.all( [ accCreator, ...accBidders ].map(async (acc, i) => {
    acc.setDebugLabel(names[i]);
  }));

  const showBalance = async (acc, i) => {
    const amt = await stdlib.balanceOf(acc);
    const amtNFT = await stdlib.balanceOf(acc, theNFT.id);
    console.log(`${names[i]} has ${stdlib.formatCurrency(amt)} ${stdlib.standardUnit} and ${amtNFT} of the NFT`);
  };

  const ctcCreator = accCreator.contract(backend);

  await Promise.all([
    (async () => {
      await showBalance(accCreator, 0);
      const n = names[0];
      await backend.Creator(ctcCreator, {
        getSale: () => {
          console.log(`${n} sets parameters of sale`);
          return [ theNFT.id, stdlib.parseCurrency(2), 30 ]
        },
        seeBid: (who, bid) => {
          console.log(`${n} saw that ${stdlib.formatAddress(who)} bid ${stdlib.formatCurrency(bid)}`);
        },
        timeout: () => {
          console.log(`${n} observes the auction has hit the timeout`);
        },
        showOutcome: (winner) => {
          console.log(`${n} saw that ${stdlib.formatAddress(winner)} won`);
        },
      });
      await showBalance(accCreator, 0);
    })(),
    ...accBidders.map(async (acc, i) => {
      await showBalance(acc, i+1);
      const n = names[i+1];
      const ctc = acc.contract(backend, ctcCreator.getInfo());
      const bid = stdlib.parseCurrency(Math.random() * 10);
      let IWon = false;
      console.log(`${n} decides to bid ${stdlib.formatCurrency(bid)}`);
      await backend.Bidder(ctc, {
        showOutcome: (winner) => {
          console.log(`${n} saw that ${stdlib.formatAddress(winner)} won`);
          IWon = stdlib.addressEq(winner, acc);
        },
        seeParams: async ([nftId, reservePrice, end]) => {
          console.log(`${n} sees that the NFT is ${nftId}, the reserve price is ${stdlib.formatCurrency(reservePrice)}, and that they have until ${end} to bid`);
          await acc.tokenAccept(nftId);
        },
        getBid: (currentPrice) => {
          if ( currentPrice.lt(bid) ) {
            console.log(`${n} bids ${stdlib.formatCurrency(bid)} against ${stdlib.formatCurrency(currentPrice)}`);
            return ['Some', bid];
          } else {
            console.log(`${n} does not bid because ${stdlib.formatCurrency(currentPrice)} is too high`);
            return ['None', null];
          }
        },
      });
      await showBalance(acc, i+1);
      if ( ! IWon ) {
        await theNFT.optOut(acc);
      }
      return;
    },
  )]);
})();
