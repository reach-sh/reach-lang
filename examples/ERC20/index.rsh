// ERC-20 Specification
// https://eips.ethereum.org/EIPS/eip-20
//
// ERC-165
// https://eips.ethereum.org/EIPS/eip-165
'reach 0.1';

const ERC165 = mixin({
  IDs: [ Bytes.fromHex('0x01ffc9a7'), ],
  View: [{
    supportsInterface: Fun([Bytes(4)], Bool),
  }],
});

const ERC20 = mixin({
  //IDs: [ Bytes.fromHex('0xTODO'), ],
  View: [{
    name: Fun([], StringDyn),
    symbol: Fun([], StringDyn),
    // TODO Decimals should return uint8
    decimals: Fun([], UInt),
    totalSupply: Fun([], UInt),
    balanceOf: Fun([Address], UInt),
    allowance: Fun([Address, Address], UInt),
  }],
  Events: [{
    Transfer: [Address, Address, UInt],
    Approval: [Address, Address, UInt],
  }],
  API: [{
    transfer: Fun([Address, UInt], Bool),
    transferFrom: Fun([Address, Address, UInt], Bool),
    approve: Fun([Address, UInt], Bool),
  }],
});

export const main = Reach.App(() => {
  setOptions({ connectors: [ETH] });

  // TODO - mix in ERC165 after I calculate the return code.
  const { IDs, View: V, Events: E, API: I } = ERC20();

  const D = Participant('Deployer', {
    meta: Object({
      name: StringDyn,
      symbol: StringDyn,
      decimals: UInt,
      totalSupply: UInt,
      zeroAddress: Address,
    }),
    deployed: Fun([Contract], Null),
  });


  init();

  D.only(() => {
    const { name, symbol, decimals, totalSupply, zeroAddress } = declassify(interact.meta);
  })
  D.publish(name, symbol, decimals, totalSupply, zeroAddress).check(() => {
    // TODO - decimals is supposed to be a uint8.  I'm not certain how to do that, but let's at least enforce the value range.
    check(decimals < 256);
  });

  D.interact.deployed(getContract());

  V.name.set(() => name);
  V.symbol.set(() => symbol);
  V.decimals.set(() => decimals);
  V.totalSupply.set(() => totalSupply);

  // TODO - enable this once I mix in ERC165.
  //const IDsArray = array(Bytes(4), IDs);
  //V.supportsInterface.set(IDsArray.includes);

  const balances = new Map(Address, UInt);
  const allowances = new Map(Tuple(Address, Address), UInt);

  balances[D] = totalSupply;
  E.Transfer(zeroAddress, D, totalSupply);

  const [ ] =
    parallelReduce([ ])
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
      })
      .while(true)
      .invariant(balance() == 0)
      .define(() => {
        const transfer_ = (from_, to, amount) => {
          balances[from_] = balanceOf(from_) - amount;
          balances[to]    = balanceOf(to) + amount;
          E.Transfer(from_, to, amount);
        }
      })
      .api_(I.transfer, (to, amount) => {
        check(to != zeroAddress, "ERC20: Transfer to zero address");
        check(balanceOf(this) >= amount,
              "amount must not be greater than balance");
        return [ (k) => {
          transfer_(this, to, amount);
          k(true);
          return [ ];
        }];
      })
      .api_(I.transferFrom, (from_, to, amount) => {
        check(from_ != zeroAddress, "ERC20: Transfer from zero address");
        check(to != zeroAddress, "ERC20: Transfer to zero address");
        check(balanceOf(from_) >= amount,
              "amount must not be greater than balance");
        check(allowance(from_, this) >= amount,
              "amount must not be greater than allowance");
        return [ (k) => {
          transfer_(from_, to, amount);
          const newAllowance = allowance(from_, this) - amount;
          allowances[[from_, this]] = newAllowance;
          // This is not required by the EIP, but OpenZeppelin emits this event.  So in order to have this contract and OpenZeppelin ERC20 pass the same test suite that checks for this event, I need to add it here.
          E.Approval(from_, this, newAllowance);
          k(true);
          return [ ];
        }];
      })
      .api_(I.approve, (spender, amount) => {
        check(spender != zeroAddress, "ERC20: Approve to zero address");
        return [ (k) => {
          allowances[[this, spender]] = amount;
          E.Approval(this, spender, amount);
          k(true);
          return [ ];
        }];
      })
  ;
  commit();
  exit();
});

