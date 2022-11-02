'reach 0.1';

export const main = Reach.App(() => {
  setOptions({ connectors: [ETH] });
  const D = Participant('Deployer', {
    meta: Object({
      name: StringDyn,
      symbol: StringDyn,
      decimals: UInt,
      totalSupply: UInt,
      zeroAddress: Address,
    }),
    launched: Fun([Contract], Null),
  });
  const A = API({
    transfer: Fun([Address, UInt], Bool),
    transferFrom: Fun([Address, Address, UInt], Bool),
    approve: Fun([Address, UInt], Bool),
  });
  const V = View({
    name: Fun([], StringDyn),
    symbol: Fun([], StringDyn),
    decimals: Fun([], UInt),
    totalSupply: Fun([], UInt),
    balanceOf: Fun([Address], UInt),
    allowance: Fun([Address, Address], UInt),
  });
  const E = Events({
    Transfer: [Address, Address, UInt],
    Approval: [Address, Address, UInt],
  });
  init();
  D.only(() => {
    const {name, symbol, decimals, totalSupply, zeroAddress} = declassify(interact.meta);
  });
  D.publish(name, symbol, decimals, totalSupply, zeroAddress).check(() => {
    check(decimals < 256, 'decimals fits in UInt8');
  });
  D.interact.launched(getContract());

  V.name.set(() => name);
  V.symbol.set(() => symbol);
  V.decimals.set(() => decimals);
  V.totalSupply.set(() => totalSupply);

  const balances = new Map(Address, UInt);
  const allowances = new Map(Tuple(Address, Address), UInt);

  balances[D] = totalSupply;
  E.Transfer(zeroAddress, D, totalSupply);

  const [] = parallelReduce([])
  .define(() => {
    const balanceOf = (owner) => {
      const m_bal = balances[owner];
      return fromSome(m_bal, 0);
    }
    V.balanceOf.set(balanceOf);
    const allowance = (owner, spender) => {
      const m_bal = allowances[[owner, spender]];
      return fromSome(m_bal, 0);
    }
    V.allowance.set(allowance);
    const transfer_ = (from_, to, amount) => {
      balances[from_] = balanceOf(from_) - amount;
      balances[to] = balanceOf(to) + amount;
      E.Transfer(from_, to, amount);
    }
  })
  .invariant(balance() == 0)
  .while(true)
  .api_(A.transfer, (to, amount) => {
    check(to != zeroAddress, 'ERC20: Transfer to zero address');
    check(balanceOf(this) >= amount, "amount must not be greater than balance");
    return[(k) => {
      transfer_(this, to, amount);
      k(true);
      return [];
    }];
  })
  .api_(A.transferFrom, (from_, to, amount) => {
    check(from_ != zeroAddress, "ERC20: Transfer from zero address");
    check(to != zeroAddress, "ERC20: Transfer to zero address");
    check(balanceOf(from_) >= amount, "amount must not be greater than balance");
    check(allowance(from_, this) >= amount, "amount must not be greater than allowance");
    return[ (k) => {
      transfer_(from_, to, amount);
      const newAllowance = allowance(from_, this) - amount;
      allowances[[from_, this]] = newAllowance;
      E.Approval(from_, this, newAllowance);
      k(true);
      return [];
    }];
  })
  .api_(A.approve, (spender, amount) => {
    check(spender != zeroAddress, "ERC20: Approve to zero address");
    return [ (k) => {
      allowances[[this, spender]] = amount;
      E.Approval(this, spender, amount);
      k(true);
      return [];
    }];
  });
  commit();
  exit();
});
