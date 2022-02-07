'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    // Alice tells the contract the token IDs of CKN and EGG
    getTokenIds: Fun([], Tuple(Token, Token)),
    // Alice tells the contract how much CKN and EGG she will donate to Bob
    getDonation: Fun([], Tuple(UInt, UInt)),
  });
  const B = Participant('Bob', {
    // Bob is shown the amount of donated CKN and EGG (just for informational purposes)
    showDonation: Fun([UInt, UInt], Null),
  });
  init();

  A.only(() => {
    const [ckn, egg] = declassify(interact.getTokenIds());
    const [donatedCkn, donatedEgg] = declassify(interact.getDonation());
    assume(ckn != egg);
  });
  A.publish(ckn, egg, donatedCkn, donatedEgg);
  commit();

  A.pay([[donatedCkn, ckn], [donatedEgg, egg]]);
  commit();

  B.interact.showDonation(donatedCkn, donatedEgg);
  B.publish();

  transfer([[donatedCkn, ckn], [donatedEgg, egg]]).to(B);
  commit();

  exit();
});
