'reach 0.1'


export const main = Reach.App(() => {
  const iface = {
    ready: Fun([], Null),
    getAddress: Fun([], Address),
    getAnswerKey: Fun([], Tuple(UInt, UInt)),
    ...hasConsoleLogger,
  }
  const A = Participant('Alice', iface);
  const B = Participant('Bob', iface);
  init();

  A.publish()
  commit()
  A.interact.ready()

  B.only(() => {
    const a1 = declassify(interact.getAddress());
  })
  B.publish(a1);

  const mc1 = Contract.fromAddress(a1);
  const v1 = mc1.match({
    None: () => {return 0},
    Some: (addr) => {return 1},
  })
  commit();
  B.interact.log(v1);

  B.only(() => {
    const a2 = declassify(interact.getAddress());
  })
  B.publish(a2);

  const mc2 = Contract.fromAddress(a2);
  const v2 = mc2.match({
    None: () => {return 0},
    Some: (addr) => {return 1},
  })
  commit();
  B.interact.log(v2);

  B.only(() => {
    const [bobV1, bobV2] = declassify(interact.getAnswerKey());
  })
  B.publish(bobV1, bobV2).check(() => {
    check(bobV1 == v1, "v1 matches");
    check(bobV2 == v2, "v2 matches");
  });
  commit();

})

