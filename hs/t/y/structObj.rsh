'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const MaxMeasures = 4;
  const VoteMeta = Struct([
    ["period", UInt],
    ["measures", UInt],
    ["choices", Array(UInt, MaxMeasures)],
    ["open", Bool],
  ]);
  const V = View({ m: VoteMeta });
  init();

  A.publish();
  const meta0 = {
    open: true,
    period: 0,
    measures: 0,
    choices: Array.replicate(MaxMeasures, 0),
  };
  V.m.set(VoteMeta.fromObject(meta0));
  commit();

  A.publish();
  commit();

  exit();
});
