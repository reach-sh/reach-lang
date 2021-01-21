'reach 0.1';

// Allow multiple payments/currencies in parallel_reduce
// Depends on: non-network_token.rsh

const N = 2;

export const main =
  Reach.App(
    {},
    [
      ['A', { amts: Array(UInt, N), shouldPay: Fun([], Bool) }],
      Array(Token, N)
    ],
    (A, tokens) => {
      A.publish();

      const publishE = () => ({
        msg: declassify(interact.amts()),
        when: declassify(interact.shouldPay())
      });

      // Accept some array/tuple type like [(Token, UInt)]
      const payE = (amtIns) => Array.zip(tokens, amtIns);

      const consensusE = (amtIns) => { return [ false, amtIns ]; };

      const [ keepGoing, amtIns ] =
        parallel_reduce([ true, Array.replicate(N, 0) ])
          .while(keepGoing)
          .invariant(true)
          .case(A, publishE, payE, consensusE)
          .timeout(100, () => {
            A.publish();
            return [ false ];
          });

      Array.zip(tokens, amtIns)
        .map(([ token, amtIn ]) =>
          transfer(amtIn).currency(token).to(A));
      commit();
      exit();
    });
