'reach 0.1';

export const main = Reach.App(() => {
  const D = Participant('Deployer', {
    getTuple: Fun([], Tuple(UInt, UInt, Address, UInt)),
    deployed: Fun([Contract], Null),
  });
  const V = View({
    vs1: Fun([UInt], Bool),
    vs2: Fun([UInt], Bool),
    vs3: Fun([UInt], Bool),
    vd1: Fun([UInt], Bool),
    vd2: Fun([UInt], Bool),
    vd3: Fun([UInt], Bool),
  })

  const staticTup = [1, "hi", 27, 14];
  init();

  D.only(() => {
    const dynamicTup = declassify(interact.getTuple());
  })
  D.publish(dynamicTup);

  D.interact.deployed(getContract());

  V.vs1.set(staticTup.includes);
  V.vs2.set((x) => staticTup.includes(x))
  V.vs3.set((x) => Tuple.includes(staticTup, x))
  // TODO - these dynamic versions are failing with an SMT error.
  V.vd1.set(dynamicTup.includes);
  V.vd2.set((x) => dynamicTup.includes(x))
  V.vd3.set((x) => Tuple.includes(dynamicTup, x))

  commit();

  D.publish();
  commit();
  exit();
});
