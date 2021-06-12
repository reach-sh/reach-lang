'reach 0.1';

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {});
  const Bob   = ParticipantClass('Bob', {});
  deploy();
  Alice.publish().pay(100);
  transfer(100).to(Participant('C', {}));
});
