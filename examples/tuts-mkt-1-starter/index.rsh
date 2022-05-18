'reach 0.1';

const commonInteract = {};

const sellerInteract = {};

const buyerInteract = {};

export const main = Reach.App(() => {
  const Seller = Participant('Seller', {
    ...commonInteract,
    ...sellerInteract,
  });
  const Buyer = Participant('Buyer', {
    ...commonInteract,
    ...buyerInteract,
  });
  init();
});
