'reach 0.1';
'use strict';

export const main =
  Reach.App(() => {

    const Creator = Participant('Creator', {
        getId: Fun([], UInt) });

    const Owner = API('Owner', {
        newOwner: Fun([Address], Null),
        showOwner: Fun([], Tuple(UInt, Address)) });

    const vNFT = View('NFT', {
        owner: Address });

    deploy();

    Creator.only(() => {
      const id = declassify(interact.getId()); });
    Creator.publish(id);

    var owner = Creator;
    { vNFT.owner.set(owner); };
    invariant(balance() == 0);
    while ( true ) {
      commit();

      const [ [], returningA] = call(Owner.showOwner).throwTimeout(false);
      returningA([id, owner]);

      commit();
      
      const [[n], returningB] = call(Owner.newOwner).throwTimeout(false);
      returningB(null)
      const amOwner = this == owner;
      const newOwner = amOwner ? n : owner;


      owner = newOwner;
      continue;
    }
    commit();

  });