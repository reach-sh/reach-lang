import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const run = async (borrowerPaysBack) => {
  const startingBalance = stdlib.parseCurrency(100);
  const [accLender, accBorrower] = await stdlib.newTestAccounts(2, startingBalance);
  const nft = await stdlib.launchToken(accBorrower, "suspicious watch", "SW", { supply: 1 });
  await accLender.tokenAccept(nft.id);
  
  const fmt = (amt) => stdlib.formatCurrency(amt, 2);
  const loanAmt = stdlib.parseCurrency(40);
  const interest = stdlib.parseCurrency(10);
  const deadline = 200;

  const printBalances = async () => {
    const [nwtBor, nftBor] = await stdlib.balancesOf(accBorrower, [null, nft.id]);
    const [nwtLen, nftLen] = await stdlib.balancesOf(accLender,   [null, nft.id]);
    console.log(`* Borrower has ${fmt(nwtBor)} ${stdlib.standardUnit} and ${nftBor} ${nft.name}`);
    console.log(`*   Lender has ${fmt(nwtLen)} ${stdlib.standardUnit} and ${nftLen} ${nft.name}`);
  };

  console.log("Starting balances:");
  await printBalances();
  const ctcBorrower = accBorrower.contract(backend);
  const ctcLender = accLender.contract(backend, ctcBorrower.getInfo());

  console.log(`Borrower puts up the ${nft.name} nft as collateral`);
  await stdlib.withDisconnect(Promise.all([
    ctcBorrower.p.Borrower({
      loanMade: async () => {
        console.log(`Borrower receives a loan of ${fmt(loanAmt)} ${stdlib.standardUnit}`);
        await printBalances();
        stdlib.disconnect();
      },
      loanAmt,
      interest,
      deadline,
      nft: nft.id,
    }),
    ctcLender.p.Lender({}),
  ]));

  if (borrowerPaysBack) {
    console.log(`Borrower pays off the loan plus ${fmt(interest)} ${stdlib.standardUnit} interest`);
    await ctcBorrower.a.LoanAPI.payBack();
  } else {
    console.log("Borrower is delinquent and does not pay off the loan");
    await stdlib.wait(deadline);
    await ctcLender.a.LoanAPI.takeCollateral();
  }

  console.log("Finishing balances:");
  await printBalances();
  console.log("\n");
}

await run(true);
await run(false);
