'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const User = Participant('User', hasConsoleLogger);
  deploy();
  User.publish();
  commit();
  const mAddress = Maybe(Address);
  const x = Array.replicate(100,mAddress.None());
  User.only(() => {
    interact.log(x);
    const newX = x.set(0,this);
    interact.log(newX);
  });
  exit();
});
