import { connect  } from '@reach-sh/stdlib';
const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

const randomArray = a => a[ Math.floor(Math.random() * a.length) ];
export const randomHand = () => randomArray([ 'ROCK', 'PAPER', 'SCISSORS' ]);

export const runGameWith = async (theRPS, interactWithAlice, interactWithBob, wagerInEth, escrowInEth) => {
  const stdlib = connect(uri);

  const wagerInWei = stdlib.toBN(stdlib.toWei(wagerInEth,  'ether'));
  const escrowInWei = stdlib.toBN(stdlib.toWei(escrowInEth, 'ether'));

  const { balanceOf, devnet, transfer } = stdlib;
  const { prefundedDevnetAcct         } = devnet;

  const startingBalance = stdlib.toBN(stdlib.toWei('100', 'ether'));

  const prefunder = await prefundedDevnetAcct();

  const newPlayer = async () => {
    const to = await devnet.createAndUnlockAcct();
    await transfer(to, prefunder, startingBalance);
    return stdlib.EthereumNetwork(to); };

  const alice = await newPlayer();
  const bob = await newPlayer();
  const balanceStartAlice = await balanceOf(alice);
  const balanceStartBob = await balanceOf(bob);
  const ctors = [ alice.userAddress, bob.userAddress ];

  const ctcAlice =
        await alice.deploy(theRPS.ABI, theRPS.Bytecode, ctors);
  const ctcBob =
        await bob.attach(theRPS.ABI, ctors, ctcAlice.address,
                         ctcAlice.creation_block);

  const [ outcomeBob, outcomeAlice ] =
        await Promise.all([
          theRPS.B(stdlib
                   , ctcBob
                   , interactWithBob),
          theRPS.A(stdlib
                   , ctcAlice
                   , interactWithAlice
                   , wagerInWei
                   , escrowInWei)]);

  const balanceEndAlice = await balanceOf(alice);
  const balanceEndBob = await balanceOf(bob);

  return { outcomeAlice, outcomeBob,
           balanceStartAlice, balanceStartBob,
           balanceEndAlice, balanceEndBob };
};
