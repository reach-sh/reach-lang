'reach 0.1';

const Common = {
  getX: Fun([], UInt),
  getY: Fun([], UInt),
  getYFromX: Fun([UInt], UInt),
};

export const main =
  Reach.App(
    { 'deployMode': 'firstMsg' },
    [['Alice',
      { ...Common,
        getParams: Fun([], Object({ wager: UInt,
                                    deadline: UInt })) }],
     ['Bob',
      { ...Common,
        confirmWager: Fun([UInt], Null) } ],
    ],
    (Alice, Bob) => {
      Alice.publish();
      commit();

      Bob.publish();
      commit();

      const x =
        race([
          [ [Alice, Bob],
            () => {
              this.only(() => {
                const myx = declassify(interact.getX()); });
              this.publish(myx);
              return myx; } ],
        ]);
      invariant(balance() == 0);
      commit();

      const y =
        race([
          [ [Alice, Bob],
            () => {
              this.only(() => {
                const _y =
                  fromMaybe(myx,
                    () => interact.getYFromX(x),
                    (_) => interact.getY() );
                const y = declassify(_y);
                assume(x == 2*y); });
              this.publish(y);
              require(x == 2*y);
              return y; } ],
        ]);
      invariant(balance() == 0 && x == 2*y);
      commit();

    });
