'reach 0.1';

export const main = Reach.App(() => {
  const Depositor = Participant('Depositor', { deposit: UInt });
  const Receiver  = Participant('Receiver', {});
  init();

  Depositor.only(() => {
    const depositT = declassify(interact.deposit);
    return depositT;
  });
  Depositor.pay(depositT);

  transfer(depositT).to(Receiver);
  commit();

});
