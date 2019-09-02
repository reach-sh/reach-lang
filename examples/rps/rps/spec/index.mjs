// vim: filetype=javascript

import glob                   from 'glob';
import Jasmine                from 'jasmine';
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

const panic = e =>
  console.error(e) || process.exit(1);

// TODO improve module path resolution
const importOrBail = p =>
  import(p.replace('rps/spec/', './'))
    .catch(panic);

glob('**/*-spec.mjs', (err, ss) =>
  !!err
    ? panic(err)
    : Promise
        .all(ss.map(importOrBail))
        .then(() => jasmine.execute())
        .catch(panic));
