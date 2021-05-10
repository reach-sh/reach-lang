// vi: ft=javascript
'reach 0.1';

import { fourTimesThree } from
  '@github.com:reach-sh/reach-example-package#dd713ff/src/lib.rsh';

export const main = Reach.App({}, [], () =>
  assert(fourTimesThree == 12));
