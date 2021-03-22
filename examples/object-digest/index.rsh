'reach 0.1';
'use strict';

const Obj = Object({x: UInt, y: Bool});

export const main = Reach.App(
  {}, [
    Participant('Alice', {
      ...hasRandom,
      getObj: Fun([], Obj),
    }),
    Participant('Bob', {
      showObj: Fun([Obj, Digest, UInt], Null),
    }),
  ], (Alice, Bob) => {
    Alice.only(() => {
      const obj = declassify(interact.getObj());
      const commitment = declassify(makeCommitment(interact, obj));
    });
    Alice.publish(obj, commitment);
    checkCommitment(...commitment, obj);
    commit();

    Bob.only(() => {
      interact.showObj(obj, ...commitment);
    });
  }
);
