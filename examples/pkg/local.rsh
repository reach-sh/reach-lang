'reach 0.1';

import * as func from
  '@github.com:reach-sh/reach-example-package:src/func.rsh';

const fiveTimesThree = func.timesThree(5);

export { fiveTimesThree   };
export { timesThree as t3 } from
  '@github.com:reach-sh/reach-example-package#main:src/func.rsh';
