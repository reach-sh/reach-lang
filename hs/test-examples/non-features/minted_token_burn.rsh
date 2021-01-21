'reach 0.1';

/*
  Because of Algorand, we'll need to have a built-in
  notion of a map-container-that-is-a-token and this
  would be an argument to Reach.DApp

  Depends on:
    MintedToken.totalSupply()
    MintedToken.balanceOf(Address)
    MintedToken.mint(Address, UInt)
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
        interact.showAmt(pool.balanceOf(A));
      });

      const pool2  = pool.mint(this, 2);

      A.only(() => {
        interact.showAmt(pool2.totalSupply());
        interact.showAmt(pool2.balanceOf(A));
      });

      /*
      XXX Feature: MintedToken.burn which behind the scenes does:
          balanceOf[from] = balanceOf[from] - value;
          totalSupply = totalSupply - value;
      */
      const pool3 = pool.burn(this, 2);

      A.only(() => {
        interact.showAmt(pool3.totalSupply());
        interact.showAmt(pool3.balanceOf(A));
      });

    });
