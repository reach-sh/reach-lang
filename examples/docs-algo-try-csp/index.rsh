'reach 0.1';

const MUInt = Maybe(UInt);
const makeMain = (untrustworthyMaps) => Reach.App(() => {
  setOptions({ untrustworthyMaps });
  const common = {
    get: Fun([UInt], UInt),
  };
  const A = Participant('A', common);
  init();

  A.publish();
  const m = new Map(UInt);
  commit();

  A.only(() => {
    const a = declassify(interact.get(1));
    const ap = a;
  });
  A.publish(a, ap);
  m[A] = a;
  require(m[A] == MUInt.Some(ap), "m[A] is Some(ap)");
  commit();

  A.only(() => {
    const b = declassify(interact.get(2));
  });
  A.publish(b);
  if ( untrustworthyMaps ) {
    possible(m[A] == MUInt.None(), "m[A] is unknown");
    possible(m[A] == MUInt.Some(forall(UInt)), "m[A] is unknown");
    possible(m[A] == MUInt.Some(ap), "m[A] is unknown");
  } else {
    assert(! isNone(m[A]), "m[A] isn't None");
    assert(m[A] == MUInt.Some(ap), "m[A] is Some(ap)");
  }
  delete m[A];
  commit();

  exit();
});

export const main = makeMain(true);
export const highTrust = makeMain(false);
