import { connect  } from '@reach-sh/stdlib';
const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

export const runGameWith = async (theRPS, interactWithAlice, interactWithBob, wagerInEth, escrowInEth) => {
  const stdlib = connect(uri);

  const wagerInWei = stdlib.toBN(stdlib.toWei(wagerInEth,  'ether'));
  const escrowInWei = stdlib.toBN(stdlib.toWei(escrowInEth, 'ether'));

  const { balanceOf } = stdlib;

  const startingBalance = stdlib.toBN(stdlib.toWei('100', 'ether'));

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);
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
          theRPS.B(ctcBob, interactWithBob),
          theRPS.A(ctcAlice, interactWithAlice,
                   wagerInWei, escrowInWei)]);

  const balanceEndAlice = await balanceOf(alice);
  const balanceEndBob = await balanceOf(bob);

  return { wagerInWei, outcomeAlice, outcomeBob,
           balanceStartAlice, balanceStartBob,
           balanceEndAlice, balanceEndBob };
};
