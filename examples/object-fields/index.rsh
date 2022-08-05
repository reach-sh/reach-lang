'reach 0.1';

const ot = Object({a: UInt, b: UInt});
const newobj = Object({...Object.fields(ot), c: UInt});

const st = Struct([["x", UInt], ["y", UInt]]);
const newstruct = Struct([...Struct.fields(st), ["z", UInt]]);

export const main = Reach.App(() => {
  const A = Participant('A', {
    seeObj: Fun([newobj], Null),
    seeStruct: Fun([newstruct], Null),
  });
  init();

  A.publish();
  commit();
  A.interact.seeObj({a: 1, b: 2, c: 3});
  A.interact.seeStruct(newstruct.fromObject({x: 1, y: 2, z: 3}));
  A.publish();
  commit();
});
