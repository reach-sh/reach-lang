'reach 0.1';

export const main = Reach.App(() => {
  const shared = {
    showToken: Fun(true, Null),
    didTransfer: Fun([Bool, UInt], Null),
  };
  const A = Participant('Alice', {
    getParams: Fun([], Object({
      name: Bytes(32), symbol: Bytes(8),
      url: Bytes(96), metadata: Bytes(32),
      supply: UInt,
      amt: UInt,
    })),
    ...shared,
  });
  const B = Participant('Bob', {
    ...shared,
  });
  const C = Participant('Eve', {})
  const D = Participant('Fab', {})
  init();

  A.publish();
  
  const m = new Map(UInt);
  m[A] = 1;

  D.set(A);

  commit();

  B.publish();
  m[B] = 2
  commit();

  C.publish()
  delete m[A]
  delete m[B]
  commit();

  exit();
});
