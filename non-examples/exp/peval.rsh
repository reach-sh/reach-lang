'reach 0.1 exe';

const test1 = 3;
const test2 = (() => { return 3; })();
const test3 = ((() => { return (() => { return 3; }); })())();

function main() {
  return test1 + test2 + test3; }
