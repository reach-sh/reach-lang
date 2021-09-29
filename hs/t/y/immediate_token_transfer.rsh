'reach 0.1';

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {});
  deploy();

  Alice.publish();
  const tok = new Token({ supply: 5 });
  transfer(5, tok).to(Alice);
  commit();

  Alice
    .publish()
    .pay([[ 5, tok ]]);

  tok.burn(5);
  tok.destroy();
  commit();

  exit();
});
