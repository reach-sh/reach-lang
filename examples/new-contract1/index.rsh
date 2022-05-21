'reach 0.1';

export const main1 = make({
  ETH: 'child.sol.bin',
  ALGO: {
    approval: 'child.approve.teal.tok',
    clearState: 'child.clear.teal.tok',
  },
});
export const main2 = make({
  ETH: 'child.sol.json',
  ALGO: {
    approval: 'child.approve.teal.tok',
    clearState: 'child.clear.teal.tok',
  },
});
export const main3 = make({
  ETH: 'child.sol',
  ALGO: {
    approval: 'child.approve.teal',
    clearState: 'child.clear.teal',
  },
});
export const main4 = make('build/child.main.mjs', true);
export const main5 = make('child.rsh:main', true);
import * as child from './child.rsh';
export const main6 = make(child.main, true);
