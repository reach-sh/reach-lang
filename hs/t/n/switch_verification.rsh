'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { amt : Maybe(UInt) });
  deploy();
  A.only(() => {
    const x = declassify(interact.amt);
    const u = x;
  });
  A.publish(x, u);
  switch (x) {
    case None: { }
    case Some: {
      assert(x == 5);
    }
  }
  commit();
});
