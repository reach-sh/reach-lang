import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as AUCTION from './build/auction.mjs';

const log = (msg) => () => { console.log(`${msg}`); return true; };

const showOutcomeSally = (o) => o ? 'sold' : 'unsold';
const showOutcomeBob = (o) => o ? 'won' : 'lost';

( async () => {
  const startingBalance = stdlib.toWeiBN('100', 'ether');

  const sally = await stdlib.newTestAccount(startingBalance);
  console.log(`Sally deploys the contract.`);
  const sallyCtc = await sally.deploy(AUCTION);
  const sallyP = AUCTION.Seller(stdlib, sallyCtc, { bidder: (bid, bidder) => log(`Sally: New bidder ${bidder} at ${bid}`),
                                            winner: (bid, bidder) => log(`Sally: Winnier is ${bidder} at ${bid}`) },
                                '0', stdlib.toWeiBN('1', 'ether'));

  const BOBS = 10;
  const bobPs = new Array(BOBS);
  for ( let i = 0; i < BOBS; i++ ) {
    const amt = stdlib.toWeiBN((i + 1), 'ether');
    const bob = await stdlib.newTestAccount(startingBalance);
    const bobCtc = await bob.attach(AUCTION, sallyCtc.address, sallyCtc.creation_block);
    const bobP = AUCTION.Buyer( stdlib, bobCtc, { bid: (previous_bid) => log(`Bob(${i}): Bidding ${amt} against ${previous_bid}`), amt });
    bobPs[i] = bobP; }

  const sallyO = await sallyP;
  const bobOs = await Promise.all(bobPs);

  console.log(`Sally thinks outcome is ${showOutcomeSally(sallyO)}.`);
  for ( let i = 0; i < BOBS; i++ ) {
    console.log(`Bob(${i}) thinks outcome is ${showOutcomeBob(bobOs[i])}.`); }

  console.log(`\nAuction complete\n`);
  process.exit(0); })();
