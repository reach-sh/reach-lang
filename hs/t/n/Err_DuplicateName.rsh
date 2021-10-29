'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const B = API('A', {});
  deploy();

});
