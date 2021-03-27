import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as TTT from './build/index.main.mjs';
import { ask, yesno, done } from '@reach-sh/stdlib/ask.mjs';



// Function to render the board into ASCII art
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
  let acc = null;
  let wager = null;
  let ctc = null;
  const isAlice = await ask(
      `Are you Alice?`,
      yesno
  );
  const who = isAlice ? 'ALICE' : 'BOB';
  console.log(`PLAYING PAY TO PLAY TIC-TAC-TOE AS ${who}`)
  console.log("\n\nUSE YOUR NUMERIC KEYPAD TO PLAY:\n")
  console.log(`\t╔═══╦═══╦═══╗`)
  console.log(`\t║ 7 ║ 8 ║ 9 ║`)
  console.log(`\t╠═══╬═══╬═══╣`)
  console.log(`\t║ 4 ║ 5 ║ 6 ║`)
  console.log(`\t╠═══╬═══╬═══╣`)
  console.log(`\t║ 1 ║ 2 ║ 3 ║`)
  console.log(`\t╚═══╩═══╩═══╝`)

  
  const createAcc = await ask(
      `Would you like to create an account? (only possible on devnet)`,
      yesno
  );
  if (createAcc) {
      acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));
  } else {
      const secret = await ask(
          `What is your account secret?`,
          (x => x)
      );
      acc = await stdlib.newAccountFromSecret(secret);
  }
  const deployCtc = isAlice;
  if (deployCtc) {
      const amt = await ask(
          `How much do you want to wager?`,
          stdlib.parseCurrency
      );
      wager = amt;
      ctc = acc.deploy(TTT);
      const info = await ctc.getInfo();
      console.log(`The contract is deployed as = ${JSON.stringify(info)}`);
  } else {
      const info = await ask(
          `Please paste the contract information:`,
          JSON.parse
      );
      ctc = acc.attach(TTT, info);
  }

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async () => fmt(await stdlib.balanceOf(acc));
  const before = await getBalance();
  console.log(`Your balance is ${before}`);

  var interactWith = (name) => ({
    ...stdlib.hasRandom,
    informTimeout : () => {
        console.log(`There was a timeout.`);
        process.exit(1);
    },
    getWager: () => {
      return wager;
    },
    getMove: (state, fee_mt) => {
      console.log(`${name} chooses a move from the state:${render(state)}`);
      const xs = state.xs;
      const os = state.os;
      let fee = 0;
      while ( xs && os ) {
        const choice = Math.floor( Math.random() * 9 );
        if ( ! ( xs[choice] || os[choice] ) ) {
          fee = (wager / 16) * fee_mt[choice];
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
    acceptWager : async (amt) => {
          const accepted = await ask(
              `Do you accept the wager of ${fmt(amt)}?`,
              yesno
          );
          if (accepted) {
              return;
          } else {
              process.exit(0);
          }
      }
  });

  const part = isAlice ? TTT.A : TTT.B;
  await Promise.all([part(ctc, interactWith(isAlice ? 'Alice' : 'Bob'))]);

  console.log(`Alice went from ${startAlice} to ${await getBalance(alice)}`)
  console.log(`Bob went from ${startBob} to ${await getBalance(bob)}`)
  console.log(`Done!`);
  process.exit(0);
})();
