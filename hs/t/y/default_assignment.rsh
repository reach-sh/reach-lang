'reach 0.1';

export const main =
  Reach.App({}, [], () => {
    const [ x, y = 2, z = 4 ] = [4, 1];
    assert(x + y + z == 9);
  });
