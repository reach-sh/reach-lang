'reach 0.1';

import * as lib from 'lib.rsh';
import {x, x as xRenamed} from 'lib.rsh';

export const main = Reach.App(
  {}, [], () => {
    assert(lib.x == x);
    assert(x == xRenamed);
  }
);
