import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as TTT from './build/index.main.mjs';

function render(st) {

    let o = "\n\t╔═══╦═══╦═══╗\n\t║";
    for (let i = 0; i < 9; i++) {
        if (st.xs[i] == 1) {
            o += " X ";
        } else if (st.os[i] == 1) {
            o += " O ";
        } else {
            o += "   ";
        }
        if (i != 8) {
            o += i % 3 == 2 ? "║\n\t╠═══╬═══╬═══╣\n\t║" : "║";
        }
    }
    o += "║\n\t╚═══╩═══╩═══╝\n";
    return o;
}

(async () => {
  console.clear();
  console.clear();
  const stdlib = await stdlib_loader.loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);
  const wagerAmount = stdlib.parseCurrency(5);
  const dispAmt = (x) => `${stdlib.formatCurrency(x)} ${stdlib.standardUnit}`;

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.deploy(TTT);
  const ctcBob = bob.attach(TTT, ctcAlice.getInfo());

  const getBalance = async (who) => (await stdlib.balanceOf(who)) / 10 ** 18;
  const startAlice = await getBalance(alice);
  const startBob = await getBalance(bob);

  const interactWith = (name) => ({
    ...stdlib.hasRandom,
    getWager: () => {
      console.log(`${name} publishes parameters of game: wager of ${dispAmt(wagerAmount)}`);
      return wagerAmount;
    },
    acceptWager: (givenWagerAmount) => {
      console.log(`${name} accepts parameters of game: wager of ${dispAmt(givenWagerAmount)}`);
    },
    getMove: (state, fee_mt) => {
      console.log(`${name} chooses a move from the state:${render(state)}`);
      const xs = state.xs;
      const os = state.os;
      let fee = 0;
      while ( xs && os ) {
        const choice = Math.floor( Math.random() * 9 );
        if ( ! ( xs[choice] || os[choice] ) ) {
          fee = (wagerAmount / 16) * fee_mt[choice];
          console.log(`${name} chooses move ${choice} from board Above.`);
          console.log(`fee: ${fee / 10 ** 18} * 10**18`);
          return choice;
        }
      }
      throw Error(`impossible to make a move`);
    },
    endsWith: (state) => {
      console.log(`${name} sees the final state: ${render(state)}`);
    },
    informTimeout : () => {
        console.log(`There was a timeout.`);
        process.exit(1);
    },
  });

  await Promise.all([
    TTT.A(ctcAlice, interactWith('Alice')),
    TTT.B(ctcBob, interactWith('Bob')),
  ]);

  console.log(`Alice went from ${startAlice} to ${await getBalance(alice)}`)
  console.log(`Bob went from ${startBob} to ${await getBalance(bob)}`)
  console.log(`Done!`);
  process.exit(0);
})();
