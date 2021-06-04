'reach 0.1';

// No `master` branch exists in the example repo but we're using it here
// deliberately to demonstrate the complier's "fall through to `main`" behavior
import { fourTimesThree } from
  '@github.com:reach-sh/reach-example-package#master:src/lib.rsh';

import './local.rsh';

export const main = Reach.App(() => {
  assert(fourTimesThree == 12);
  assert(fiveTimesThree == 15);
  assert(t3(6)          == 18);
});
