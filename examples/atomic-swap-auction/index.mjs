import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';

const shouldFail = async (fp) => {
  let worked = undefined;
  try {
    await fp();
    worked = true;
  } catch (e) {
    worked = false;
  }
  console.log(`\tshouldFail = ${worked}`);
  if (worked !== false) {
    throw Error(`shouldFail`);
  }
};

const N = 3;
const bidderNames = ["Alice", "Bob", "Camus"];

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();
  const conn = stdlib_loader.getConnector();

  const startingBalance = stdlib.parseCurrency(10);
  const zorkmid = await launchToken("zorkmid", "ZMD");
  const gil = await launchToken("gil", "GIL");

  const accAuctioneer = (await stdlib.newTestAccount(startingBalance)).setDebugLabel("Auctioneer");
  const accBidders = await Promise.all(
    Array.from({ length: N }, () => stdlib.newTestAccount(startingBalance))
  );
  accBidders.forEach((accBidder, i) => accBidder.setDebugLabel(bidderNames[i]));

  if ( conn == 'ETH' ) {
    const myGasLimit = 5000000;
    accAuctioneer.setGasLimit(myGasLimit);
    accBidders.forEach(accBidder => accBidder.setGasLimit(myGasLimit));
  } else if ( conn == 'ALGO' ) {
    console.log(`Demonstrating need to opt-in on ALGO`);
    await shouldFail(async () => await zorkmid.mint(accAuctioneer, startingBalance));
    console.log(`Opt-ing in on ALGO`);
    await stdlib.transfer(accAuctioneer, accAuctioneer, 0, zorkmid.id);
    await stdlib.transfer(accAuctioneer, accAuctioneer, 0, gil.id);
    await Promise.all([
      accBidders.forEach(async accBidder => {
        await stdlib.transfer(accBidder, accBidder, 0, zorkmid.id);
        await stdlib.transfer(accBidder, accBidder, 0, gil.id);
      })
    ]);
  }

  // Fund auctioneer with ZMD and bidders with GIL
  await zorkmid.mint(accAuctioneer, startingBalance.mul(2));
  for (let i = 0; i < accBidders.length; ++i) {
    const accBidder = accBidders[i];
    await gil.mint(accBidder, startingBalance.mul(2));
  }

  if ( conn == 'ALGO' ) {
    console.log(`Demonstrating opt-out on ALGO`);
    console.log(`\tAuctioneer opts out`);
    await zorkmid.optOut(accAuctioneer);
    console.log(`\tAuctioneer can't receive mint`);
    await shouldFail(async () => await zorkmid.mint(accAuctioneer, startingBalance));
    console.log(`\tAuctioneer re-opts-in`);
    await stdlib.transfer(accAuctioneer, accAuctioneer, 0, zorkmid.id);
    console.log(`\tAuctioneer can receive mint`);
    await zorkmid.mint(accAuctioneer, startingBalance);

    for (let i = 0; i < accBidders.length; ++i) {
      const accBidder = accBidders[i];
      const who = bidderNames[i];
      console.log(`\t${who} re-opts-in`);
      await stdlib.transfer(accBidder, accBidder, 0, gil.id);
      console.log(`\t${who} can receive mint`);
      await gil.mint(accBidder, startingBalance);
    }
  }

  const fmt = (x) => stdlib.formatCurrency(x, 4);

  const amtA = stdlib.parseCurrency(1);
  const reservePrice = stdlib.parseCurrency(3);

  console.log(`\nStarting auction of ${fmt(amtA)} ${zorkmid.sym} for ${gil.sym}`);

  const getBalance = async (tokenX, who) => {
    const amt = await tokenX.balanceOf(who);
    return `${fmt(amt)} ${tokenX.sym}`; };

  const getBalances = async (who) =>
    `${await getBalance(zorkmid, who)} & ${await getBalance(gil, who)}`;

  // Display balances
  const beforeAuctioneer = await getBalances(accAuctioneer);
  console.log(`Auctioneer has ${beforeAuctioneer}`);
  const biddersBalances = accBidders.map(_ => ({ before: 0, after: 0}));
  for (let i = 0; i < accBidders.length; ++i) {
    const accBidder = accBidders[i];
    const who = bidderNames[i];
    biddersBalances[i].before = await getBalances(accBidder);
    console.log(`${who} has ${biddersBalances[i].before}`);
  }

  // Deploy
  console.log(`Auctioneer will deploy the Reach DApp.`);
  const ctcAuctioneer = accAuctioneer.deploy(backend);

  const common = (who) => ({
    showOutcome: (outcome) => {
      console.log(`${who} saw outcome: ${outcome}`);
    }
  });

  const ctcInfo = ctcAuctioneer.getInfo();


  const bidderBackends = accBidders.map((accBidder, i) => {
    const who = bidderNames[i];
    console.log(`${who} attaches to the Reach DApp.`);
    const ctcBidder = accBidder.attach(backend, ctcInfo);
    return backend.Bidder(ctcBidder, {
      ...common(who),
      getBid: (cp) => {
        const bid = cp.add(stdlib.parseCurrency(1));
        console.log(`${who} tries to bid ${fmt(bid)} based on price of ${fmt(cp)}`);
        return ['Some', bid];
      },
      showBid: (isS, s) => console.log(`${who} saw their bid: ${s} , is some: ${isS}`)
    });
  });

  const auctioneerBackend =
    backend.Auctioneer(ctcAuctioneer, {
      ...common('Auctioneer'),
      getSwap: () => {
        console.log(`Auctioneer proposes swap`);
        return [ zorkmid.id, amtA, gil.id, reservePrice, 3 * 3 ]; },
      showAuctionStart: () => console.log(`Auction starts`)
    });

  await Promise.all([...bidderBackends, auctioneerBackend]);

  const afterAuctioneer = await getBalances(accAuctioneer);
  console.log(`Auctioneer went from ${beforeAuctioneer} to ${afterAuctioneer}`);
  for (let i = 0; i < accBidders.length; ++i) {
    const accBidder = accBidders[i];
    const who = bidderNames[i];
    biddersBalances[i].after = await getBalances(accBidder);
    console.log(`${who} went from ${biddersBalances[i].before} to ${biddersBalances[i].after}`);
  }

})();
