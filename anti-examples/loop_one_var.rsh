'reach 0.1';

export const main = Reach.App(
  {}, [["A", {}]], (A) => {
    A.publish();
    var [ x ] = [ 1 ];
    invariant(true);
    while(x < 2) {
      // Weird JS parse error on this. "SimpleAssignToken"
      [ x ] = [ x + 1 ];
      continue;
    }
    commit();
    return 0;
  }
);
