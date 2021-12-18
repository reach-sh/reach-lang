'reach 0.1';

const D = Data({ Yo: Object({ a3: UInt }) });

export const main = Reach.App(() => {
  const P = Participant('P', {});
  const V = View({ o1: Object({ a1: UInt }) });
  const A = API({ o2: Fun([Array(Object({ a2: UInt }), 2)], Null) });
  const B = API({ o3: Fun([], D) });
  deploy();

  P.publish();
  commit();

  const [o2d,, k] = call(A.o2);
  k(null);
  commit();

  const [o3d, k2] = call(B.o3);
  k2(D.Yo({ a3: 4 }));
  commit();

});
