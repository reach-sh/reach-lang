'reach 0.1';
const make = (MT) => Reach.App(() => {
  const A = Participant('A', {
    v: MT,
    fi: Fun([MT], Null),
    fo: Fun([], MT),
  });
  const P = API('P', {
    fi: Fun([MT], Null),
    fo: Fun([], MT),
  });
  const V = View('V', {
    v: MT,
    fi: Fun([MT], Null),
    fo: Fun([], MT),
  });
  const E = Events('E', {
    e: [MT],
  });
  init();
  A.publish();
  commit();
  const [ [i], ik ] = call(P.fi);
  ik(null);
  commit();
  const [ [], ok ] = call(P.fo);
  ok(i);
  commit();
  exit();
});
export const mt_struct = make(Struct([]));
export const mt_object = make(Object({}));
export const mt_tuple = make(Tuple());
export const mt_array = make(Array(UInt, 0));
