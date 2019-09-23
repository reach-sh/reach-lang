import Jasmine from 'jasmine';
import JasmineConsoleReporter from 'jasmine-console-reporter';
const jasmine = new Jasmine();
jasmine.env.clearReporters();

jasmine.addReporter(new JasmineConsoleReporter(
  { colors:     false
  , cleanStack: true
  , verbosity:  4
  , listStyle:  'indent'
  , activity:   'dots'
  }));

import('./spec/index.mjs')
  .then(() => jasmine.execute())
  .catch(e => console.error(e));
