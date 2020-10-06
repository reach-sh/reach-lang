'reach 0.1';

export const main =
  Reach.App(
    {},
    [],
    () => {
      assert(true, 4);
      exit();
    } );
