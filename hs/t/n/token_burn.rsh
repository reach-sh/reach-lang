'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  setOptions({ verifyPerConnector: true });
  init();

  A.publish();
  const amt = UInt.max;
  const tok = new Token({ supply: amt });
  commit();

  A.publish();
  transfer(20, tok).to(A);
  commit();

  A.publish();
  tok.burn(amt);
  tok.destroy();
  commit();

});
