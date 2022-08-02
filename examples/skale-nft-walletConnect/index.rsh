'reach 0.1';
'use strict';

export const main = 
  Reach.App(() => {
  
    const Maker = Participant('Maker', {
	  nftID: Fun([], UInt),
	  firstNFT: Token,
	  price: UInt,
	  amt: UInt
	});
	  
	const Buyer = Participant('Buyer', {
	  purchase: Fun([UInt], Null) 
	});
	  
	init();
	
	Maker.only(() => {
	  const nftID = declassify(interact.nftID());
	  const firstNFT = declassify(interact.firstNFT);
	  const price = declassify(interact.price);
	  const amt = declassify(interact.amt);
    });
	
	Maker.publish(nftID, firstNFT, price, amt)
	commit();
	Maker.pay([[amt, firstNFT]]);
	commit();
	
    Buyer.publish()
      .pay(price);
    Buyer.only(() => {
	  interact.purchase(price);
	});

	transfer([[amt, firstNFT]]).to(Buyer);
	transfer(price).to(Maker);

	commit();
	
	exit();
	});
	  