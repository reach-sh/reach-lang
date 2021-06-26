'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...hasConsoleLogger,
  });
  deploy();

  A.interact.log(true);
  A.interact.log(1);
  A.interact.log([1, true]);
  A.interact.log({x: 1, y: true});
  A.interact.log(Maybe(UInt).Some(5));

  exit();
});
