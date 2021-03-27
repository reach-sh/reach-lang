import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';



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
  const stdlib = await stdlib_loader.loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

  const wagerAmount = stdlib.parseCurrency(5);
  const getBalance = async (who) => (await stdlib.balanceOf(who)) / 10 ** 18;
  const startAlice = await getBalance(alice);
  const startBob = await getBalance(bob);
  const dispAmt = (x) => `${stdlib.formatCurrency(x)} ${stdlib.standardUnit}`;

  const interactWith = (name) => ({
    ...stdlib.hasRandom,
    getWager: () => {
      console.log(`${name} publishes parameters of game: wager of ${dispAmt(wagerAmount)}`);
      return wagerAmount;
    },
    acceptWager: (givenWagerAmount) => {
      console.log(`${name} accepts parameters of game: wager of ${dispAmt(givenWagerAmount)}`);
    },
    getMove: (state) => {
      console.log(`${name} chooses a move from the state:${render(state)}`);
      const xs = state.xs;
      const os = state.os;
      while ( xs && os ) {
        const i = Math.floor( Math.random() * 9 );
        if ( ! ( xs[i] || os[i] ) ) {
          return i;
        }
      }
      throw Error(`impossible to make a move`);
    },
    endsWith: (state) => {
      console.log(`${name} sees the final state: ${render(state)}`);
    },
  });

  await Promise.all([
    backend.A(ctcAlice, interactWith('Alice')),
    backend.B(ctcBob, interactWith('Bob')),
  ]);

  console.log(`Alice went from ${startAlice} to ${await getBalance(accAlice)}`)
  console.log(`Bob went from ${startBob} to ${await getBalance(accBob)}`)
  console.log(`Done!`);
  process.exit(0);
})();
