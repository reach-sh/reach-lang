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

  
  const createAcc = await ask(
      `Would you like to create an account? (only possible on devnet)`,
      yesno
  );
  if (createAcc) {
      let acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));
  } else {
      const secret = await ask(
          `What is your account secret?`,
          (x => x)
      );
      let acc = await stdlib.newAccountFromSecret(secret);
  }
  const deployCtc = isAlice;
  if (deployCtc) {
      const amt = await ask(
          `How much do you want to wager?`,
          stdlib.parseCurrency
      );
      let wager = amt;
      let ctc = acc.deploy(TTT);
      const info = await ctc.getInfo();
      console.log(`The contract is deployed as = ${JSON.stringify(info)}`);
  } else {
      const info = await ask(
          `Please paste the contract information:`,
          JSON.parse
      );
      let ctc = acc.attach(TTT, info);
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
        getMove: async (board, fee_mt) => {
            // Log that the player has chosen a move (they will very soon)
            console.log(`${render(board)}\n${name}'s turn.`);
            // localize the X and O boards
            //return Math.floor( Math.random() * 9);
            const xs = board.xs;
            const os = board.os;
            let fee = 0;
            while (true) {
                let choice = 99;
                    // Prompt user for a position to take
                    const POSITIONS = {
                        "7": 0,
                        "8": 1,
                        "9": 2,
                        "4": 3,
                        "5": 4,
                        "6": 5,
                        "1": 6,
                        "2": 7,
                        "3": 8,
                    };
                    choice = await ask(`${name} what position will you play?`, (x) => {
                        console.log(`You played ${x}`);
                        const position = POSITIONS[x];
                        if (position == null) {
                            throw Error(`Not a valid position ${position}`);
                        }
                        return position;
                    });
                    console.log(`Which maps to ${choice}`);

                // If the place is not already occupied..
                if (xs[choice] + os[choice] == 0) {
                    // Calculate the theoretical fee for that place
                    fee = (wager / 16) * fee_mt[choice];
                    console.log(`${name} chooses move ${choice} from board Above.`);
                    console.log(`fee: ${fee / 10 ** 18} * 10**18`);
                    // Return the place chosen to move to.
                    return choice;
                } else {
                    console.log("Invalid Choice Try Again")
                }
            }
            // FIXME: This never happens.
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
