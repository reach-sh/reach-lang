'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const Creator = Participant('Creator', {
    transferTo: Fun([], Address),
  });
  const Owner = ParticipantClass('Owner', {
    notifyReceive: Fun([], Null),
    notifyRenewNeeded: Fun([], Null),
    notifyRenewSuccess: Fun([], Null),
    notifyRenewFail: Fun([], Null),
  });
  const vNFT = View('NFT', { owner: Address });

  init();

  Creator.publish();

  // const ownerOnly = (currentOwner, fn) =>
  //   Owner.only(() => { if (this == currentOwner) { fn(interact); } });

  var owner = Creator;
  invariant(true);
  while (true) {
    vNFT.owner.set(owner);

    if (owner == Creator) {
      // Creator has the NFT
      // Either the contract is brand new or the dead man's switch has triggered
      commit();

      // Creator gives the NFT to someone else to arm the switch
      Creator.only(() => {
        const recipient = declassify(interact.transferTo());
      });
      Creator.publish(recipient);

      Owner.only(() => { if (this == recipient) { interact.notifyReceive(); } })

      owner = recipient;
      continue;
    } else {
      // Someone other than the owner has the NFT
      commit();

      // The owner is 'safe' for 3 seconds
      wait(relativeSecs(3));
      
      // The owner must renew their ownership
      Owner.only(() => {
        const amOwner = this == owner;
        if (amOwner) { interact.notifyRenewNeeded(); }
      });
      Owner.publish()
        .when(amOwner)
        .timeout(relativeSecs(3), () => {
          // The owner failed to appear, ownership goes back to Creator
          Owner.only(() => { if (amOwner) { interact.notifyRenewFail(); } });
          Creator.publish();
          owner = Creator;
          continue;
        });

      require(this == owner);
      Owner.only(() => { if (amOwner) { interact.notifyRenewSuccess(); } });

      continue;
    }
  }

  commit();
  assert(false);
});
