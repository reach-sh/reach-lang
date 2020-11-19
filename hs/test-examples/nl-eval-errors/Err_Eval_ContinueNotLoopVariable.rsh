'reach 0.1';

export const main = Reach.App(
  {}, [["A", {}]], (A) => {
    A.publish();
    var [x] = [1];
    invariant(true);
    while(x < 2) {
      [ x, y ] = [ x + 1, x ];
      continue;
    }
    commit();
  }
);
