'reach 0.1';

const make = (cc, isReach = false) => Reach.App(() => {
  const A = Participant('A', {
    ...hasConsoleLogger,
  });
  const ChildCode = ContractCode(cc);
  init();
  A.publish();
  const childNew = Contract(ChildCode, {
    ALGO: isReach ? {
      globalBytes: 1
    } : {
      globalUints: 2,
    }
  }).new;
  const child = isReach ? childNew() : childNew(2);
  const childo = remote(child, {
    'f': Fun([UInt], UInt),
  });
  commit();

  const go = (isLast = false) => {
    A.publish();
    const { f } = childo;
    const x = ((isLast && isReach) ? f.ALGO({
    }) : f)(1);
    A.interact.log(x);
    commit();
  };

  go();
  go();
  go(true);

  exit();
});

// We can parse compiled output
export const main1 = make({
  ETH: 'child.sol.bin',
  ALGO: {
    approval: 'child.approve.teal.tok',
    clearState: 'child.clear.teal.tok',
  },
});

// (Including the JSON that Solidity produces)
export const main2 = make({
  ETH: 'child.sol.json:child.sol:Contract',
  ALGO: {
    approval: 'child.approve.teal.tok',
    clearState: 'child.clear.teal.tok',
  },
});

// And we can compile for ourselves
export const main3 = make({
  ETH: 'child.sol:Contract',
  ALGO: {
    approval: 'child.approve.teal',
    clearState: 'child.clear.teal',
  },
});

// But we can also look at Reach's outputs
//export const main4 = make('build/child.main.mjs', true);

// Or call Reach ourselves
//export const main5 = make('child.rsh:main', true);

// Or directly import the object
//import * as child from './child.rsh';
//export const main6 = make(child.main, true);
