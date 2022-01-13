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
      const id = declassify(interact.getId()); 
      const originalCreator = this;
      });
    Creator.publish(id, originalCreator);
 
    const [owner] = parallelReduce([originalCreator
    ]).define(()=>{
        vNFT.owner.set(owner); 
    }).invariant(balance() == 0)
    .while(true)
    .api(Owner.showOwner, (returnFunc) => {
        returnFunc([id, owner]);
        return [owner]
    }).api(Owner.newOwner,
    (_) => {
        assume(this == owner )
    }, (_) =>{
        return 0
    }, (newOwnerAdress, returnFunc)=>{
        require(this == owner);
        returnFunc(null);
        return[newOwnerAdress]
    }).timeout(relativeTime(10), () => {
      Anybody.publish();
      return [owner];
    });;
    commit();
  });