'reach 0.1';

const commonInteract = {};
const sellerInteract = {
  ...commonInteract,
};
const buyerInteract = {
  ...commonInteract,
};

export const main = Reach.App(() => {
  const S = Participant('Seller', sellerInteract);
  const B = Participant('Buyer', buyerInteract);
  init();

  exit();
});
