'reach 0.1';

const MUInt = Maybe(UInt);
const Point = Data(
  {D2: Object({x: UInt, y: UInt}),
   D3: Object({x: UInt, y: UInt, z: UInt})});

export const main = Reach.App(
  {},
  [Participant('A', { get1: Fun([], MUInt),
           put1: Fun([MUInt], Null),
           get2: Fun([], Point),
           put2: Fun([Point], Null) })],
  (A) => {
    A.only(() => {
      const mi = declassify(interact.get1());
      const i = MUInt.Some(42);
      const iv = fromMaybe(i, (() => assert(false)), ((x) => x));
      const i2 = (() => { switch ( mi ) {
        case None: return iv;
        case Some: return iv + mi; } })();
      const mi2 = MUInt.Some(i2);
      interact.put1(mi2);
      const p = declassify(interact.get2());
      const p3 = (() => { switch ( p ) {
        case D2: return {x: p.x, y: p.y, z: i2};
        case D3: return p; } })();
      const i3 = p3.x + p3.y + p3.z;
      const p3p = { ...p3, x: i3 };
      interact.put2(Point.D3(p3p)); } );
    A.publish(mi, p);

    const ci = MUInt.Some(42);
    const civ = fromMaybe(ci, (() => assert(false)), ((x) => x));
    const ci2 = (() => { switch ( mi ) {
      case None: return civ;
      case Some:
      return civ + mi; } })();
    const cp3 = (() => { switch ( p ) {
      case D2: return {x: p.x, y: p.y, z: ci2};
      case D3: return p; } })();
    const ci3 = cp3.x + cp3.y + cp3.z;
    const c3p = { ...cp3, x: ci3 };

    switch (mi) {
    case None:
      commit();

      exit();
    case Some:
      commit();

      A.only(() => {
        const x = c3p; });
      A.publish(x);
      require(x.x == ci3);
      commit();

      exit();
    }
  } );
