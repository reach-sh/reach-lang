'reach 0.1';

// Importing an ident that isn't there
import {whoops} from './sample_lib.rsh'

export const main = Reach.App(
  {}, [], () => { return 0; }
);
