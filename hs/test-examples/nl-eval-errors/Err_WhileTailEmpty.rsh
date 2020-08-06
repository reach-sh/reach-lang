'reach 0.1';

export const main = Reach.App(
  {}, [["A", {}]], (A) => {
    A.publish();

    var x = 0;
    invariant(true);
    while(x < 1) {};
    commit();
  }
);
