'reach 0.1';

import { blah as x } from 'sample_lib.rsh';

export const main = Reach.App(
  {}, [], () => {
    return x;
  }
);
