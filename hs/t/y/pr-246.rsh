'reach 0.1';

const Command = Data({
  Deposit: UInt
})

export const main = Reach.App(() => {
  deploy();
  const x = Command.Deposit(1);
  // Test that `default` does not come first in macro expansion (maintains correct order)
  const y = x.match({
    Deposit: ((a) => a),
    default: (() => 0),
  });
})
