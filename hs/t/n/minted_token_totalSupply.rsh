'reach 0.1';

/*
  Because of Algorand, we'll need to have a built-in
  notion of a map-container-that-is-a-token and this
  would be an argument to Reach.DApp
*/

export const main =
  Reach.App(
    {},
    [
      ['A', { showAmt: Fun([UInt], Null) }],
      MintedToken
    ],
    (A) => {
      A.publish();
      commit();

      A.only(() => {
        interact.showAmt(pool.totalSupply());
      });

    });
