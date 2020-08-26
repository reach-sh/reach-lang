'reach 0.1';

// TODO: fix tuple_set and then move this to
// hs/test-examples/features/arrays.rsh
export const main = Reach.App(
  {}, [], () => {
    const xs = [0, 'one', 2, 3];
    assert(xs[1] == 'one');

    const xs2 = tuple_set(xs, 1, 'ONE');
    // ^ caused by this line
    // Map.!: given key is not an element in the map
    // CallStack (from HasCallStack):
    // error, called at
    // libraries/containers/containers/src/Data/Map/Internal.hs:627:17
    // in containers-0.6.2.1:Data.Map.Internal
    assert(xs[1] == 'ONE');
  }
);
