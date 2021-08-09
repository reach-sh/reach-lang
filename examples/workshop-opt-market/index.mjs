import { loadStdlib } from "@reach-sh/stdlib";
import * as backend from "./build/index.main.mjs";

const numOfBuyers = 4;

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

  // Make dummy accounts for Funder, Contestants
  // and 1 Monitor(since Monitor doesn't really
  // have much use in console environment)
  const accFunder = await stdlib.newTestAccount(startingBalance);
  const accContestantArray = await Promise.all(
    Array.from({ length: numOfBuyers }, () =>
      stdlib.newTestAccount(startingBalance)
    )
  );
  const accMonitor = await stdlib.newTestAccount(0);

  const ctcFunder = accFunder.deploy(backend);
  const ctcInfo = ctcFunder.getInfo();
  const ctcMonitor = accMonitor.attach(backend, ctcInfo);

  // Test funder parameters
  const funderParams = {
    amt: stdlib.parseCurrency(20),
    deadline: 50,
  };

  // define Funder interact functions
  const Funder = (Who) => ({
    getBounty: () => {
      return funderParams;
    },
    postWager: () => {
      console.log("Post Wager");
    },
  });

  // define Contestant interact functions
  const Contestant = (i, ctc) => ({
    isSubmissionPolled: false,
    submitValue: () => {
      if (isSubmissionPolled || Math.random() < 0.5) {
        isSubmissionPolled = true;
        const value = Math.floor(Math.random() * 30);
        console.log(`Contestant ${i} submitted ${value}`);
        return ["Some", value];
      } else {
        console.log(`Contestant ${i} did not submit`);
        return ["None", null];
      }
    },
    informWinner: (winner) => {
      if (stdlib.addressEq(winner, accBuyer)) {
        console.log(`Contestant ${i} won!`);
      }
    },
    informBounty: (bountyAmt, deadline) => {
      console.log(
        `Contestant ${i} saw a bounty of ${bountyAmt} and deadline ${deadline}`
      );
    },
    informSuccess: async (status) => {
      console.log(`Contestant ${i} saw status: ${status}`);
      isSubmissionPolled;
    },
  });

  // define a dummy logging function as Monitor doesn't
  // serve any purpose in console environment
  const Monitor = (i) => ({
    seeSubmission: (addr, input, output) => {
      console.log(`Monitor ${i} saw ${addr}, ${input}, ${output}`);
    },
  });

  await Promise.all([
    backend.Funder(ctcFunder, Funder("Funder")),
    accContestantArray.map((accContestant, i) => {
      const ctcContestant = accContestant.attach(backend, ctcInfo);
      return backend.Contestant(ctcContestant, Contestant(i, ctcContestant));
    }),
    backend.Monitor(ctcMonitor, Monitor(1)),
  ]);

  // check final balances of Contestants
  for (let i = 0; i < accContestantArray.length; i++) {
    const afterBalance = await getBalance(accContestantArray[i]);
    console.log(`Contestant ${i} went to ${afterBalance}.`);
  }
})();
