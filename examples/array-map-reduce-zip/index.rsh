'reach 0.1';

const part = {
  ...hasConsoleLogger,
  getArray: Fun([], Array(UInt, 5)),
  expectMapped: Fun([Array(UInt, 5)], Null),
  expectReduced: Fun([UInt], Null),
  expectConsensusMapped: Fun([Array(UInt, 5)], Null),
  expectConsensusReduced: Fun([UInt], Null),
  expectConsensusZipped: Fun([Array(Tuple(UInt, UInt, UInt), 5)], Null),
}

export const main = Reach.App(() => {
  const A = Participant('A', {...part,});
  const B = Participant('B', {...part,});
  init();

  A.only(() => {
    const a1 = array(UInt, [0,1,2,3,4])
    const a1_m = a1.mapWithIndex((x, i) => x + i)
    const ia = declassify(interact.getArray())
    const mapped = a1_m.mapWithIndex(ia, (x, y, i) => y % 2 == 0 ? 0 : x)
    const reduced = mapped.reduce(ia, 256, (z, v1, v2) => z + (v1 < v2 ? v1 : v2))
  })
  A.publish(mapped, reduced);
  commit();

  B.only(() => {
    const ba = declassify(interact.getArray())
  })
  B.publish(ba);

  const consensusMapped = mapped.map(ba, (v1, v2) => v1 + v2);
  const consensusReduced = mapped.reduceWithIndex(ba, 0, (acc, v1, v2, i) => acc + v1 + v2 + i);
  const consensusZipped = mapped.zip(ba, array(UInt, [1,1,1,1,1]));
  commit();

  A.only(() => {
    interact.expectMapped(mapped);
    interact.expectReduced(reduced);
    interact.expectConsensusMapped(consensusMapped);
    interact.expectConsensusReduced(consensusReduced);
    interact.expectConsensusZipped(consensusZipped);
  })

})
