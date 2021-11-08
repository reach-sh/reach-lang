'reach 0.1';

const Command = Data({
  Deposit: UInt
})

export const main = Reach.App(() => {
  deploy();
  const x = Command.Deposit(1);
  const y = x.match({
    default: (() => 0),
    Deposit: ((a) => a),
  });
})
