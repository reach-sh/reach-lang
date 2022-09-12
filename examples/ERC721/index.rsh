// ERC-721 Specification
// https://eips.ethereum.org/EIPS/eip-721
//
// ERC-165
// https://eips.ethereum.org/EIPS/eip-165
'reach 0.1';


const Empty = () => {
  return { IDs: [], View: {}, Events: {}, API: {} };
};

const mixin = (args = {}) => {
  const def = (k, d) => Object.has(args, k) ? args[k] : d;
  const Base = def('Base', Empty);
  return (base = Base) => {
    const { IDs: i, View: v, Events: e, API: a } = base();
    const mapp = (f, k) => Object.has(args, k) ? f(...args[k]) : {};
    return {
      IDs: [...i, ...def('IDs', []) ],
      View: {...v, ...mapp(View, 'View')},
      Events: {...e, ...mapp(Events, 'Events')},
      API: {...a, ...mapp(API, 'API')},
    };
  };
};


const ERC165 = mixin({
  IDs: [ Bytes.fromHex('0x01ffc9a7'), ],
  View: [{
    supportsInterface: Fun([Bytes(4)], Bool),
  }],
});

const ERC721 = mixin({
  Base: ERC165,
  IDs: [ Bytes.fromHex('0x80ac58cd'), ],
  View: [{
    balanceOf: Fun([Address], UInt),
    ownerOf: Fun([UInt], Address),
    getApproved: Fun([UInt], Address),
    isApprovedForAll: Fun([Address, Address], Bool),
  }],
  Events: [{
    Transfer: [Address, Address, UInt],
    Approval: [Address, Address, UInt],
    ApprovalForAll: [Address, Address, Bool],
  }],
  API: [{
    safeTransferFrom1: Fun([Address, Address, UInt, BytesDyn], Null),
    safeTransferFrom2: Fun([Address, Address, UInt], Null),
    transferFrom: Fun([Address, Address, UInt], Null),
    approve: Fun([Address, UInt], Null),
    setApprovalForAll: Fun([Address, Bool], Null),
  }, {
    safeTransferFrom1: 'safeTransferFrom',
    safeTransferFrom2: 'safeTransferFrom',
  }],
});

const ERC721Metadata = mixin({
  Base: ERC721,
  IDs: [ Bytes.fromHex('0x5b5e139f'), ],
  View: [{
    name: StringDyn,
    symbol: StringDyn,
    tokenURI: Fun([UInt], StringDyn),
  }],
});

const ERC721Enumerable = mixin({
  Base: ERC721,
  IDs: [ Bytes.fromHex('0x780e9d63'), ],
  View: [{
    totalSupply: UInt,
    tokenByIndex: Fun([UInt], UInt),
    tokenOfOwnerByIndex: Fun([Address, UInt], UInt),
  }],
});

const ERC721TokenReceiverI = {
    onERC721Received: Fun([Address, Address, UInt, BytesDyn], Bytes(4)),
};
const ERC721TokenReceiver = mixin({
  IDs: [ Bytes.fromHex('0x150b7a02'), ],
  API: [{
    ...ERC721TokenReceiverI,
  }],
});

export const main = Reach.App(() => {
  setOptions({ connectors: [ETH] });

  const { IDs, View: V, Events: E, API: P } =
    ERC721Enumerable( ERC721Metadata );

  const D = Participant('Deployer', {
    meta: Object({
      name: StringDyn,
      symbol: StringDyn,
      tokenURI: StringDyn,
    }),
    enum: Object({
      totalSupply: UInt
    }),
    zeroAddr: Address,
    emptyBytesDyn: BytesDyn,
    deployed: Fun([Contract], Null),
  });

  const Pn = API({
    mint: Fun([Address, UInt], Null),
    burn: Fun([UInt], Null),
  });
  const I = { ...P, ...Pn };

  init();

  D.only(() => {
    const { name, symbol, tokenURI } = declassify(interact.meta);
    const { totalSupply } = declassify(interact.enum);
    const zeroAddr = declassify(interact.zeroAddr);
    const emptyBytesDyn = declassify(interact.emptyBytesDyn);
  })
  D.publish(name, symbol, tokenURI, totalSupply, zeroAddr, emptyBytesDyn);

  D.interact.deployed(getContract());

  V.name.set(name);
  V.symbol.set(symbol);
  V.totalSupply.set(totalSupply);
  const IDsArray = array(Bytes(4), IDs);
  V.supportsInterface.set(IDsArray.includes);

  const owners = new Map(UInt, Address);
  const balances = new Map(UInt);
  const tokenApprovals = new Map(UInt, Address);
  const operatorApprovals = new Map(Tuple(Address, Address), Bool);

  const [ ] =
    parallelReduce([ ])
      .define(() => {


        V.balanceOf.set((owner) => {
          check(owner != zeroAddr, "ERC721::balanceOf: Address zero is not a valid owner");
          const m_bal = balances[owner];
          return fromSome(m_bal, 0);
        });
        const tokenExists = (tokenId) => isSome(owners[tokenId]);

        const ownerOf = (tokenId) => {
          check(tokenExists(tokenId), "ERC721::ownerOf: Owner query for non-existent token");
          const m_owner = owners[tokenId];
          return fromSome(m_owner, zeroAddr);
        };
        V.ownerOf.set(ownerOf);

        const getApproved = (tokenId) => {
          check(tokenExists(tokenId), "ERC721::getApproved: approved query for non-existent token")
          const m_approval = tokenApprovals[tokenId];
          return fromSome(m_approval, zeroAddr);
        };
        V.getApproved.set(getApproved);

        const isApprovedForAll = (owner, operator) => {
          const m_approved = operatorApprovals[[owner, operator]];
          return fromSome(m_approved, false);
        };
        V.isApprovedForAll.set(isApprovedForAll);

        const isApprovedOrOwner = (spender, tokenId) => {
          check(tokenExists(tokenId), "isApprovedOrOwner: token exists");
          const owner = ownerOf(tokenId);
          return spender == owner || isApprovedForAll(owner, spender) || getApproved(tokenId) == spender;
        }

        V.tokenURI.set((tokenId) => {
          check(tokenExists(tokenId), "tokenURI: URI query for non-existent token");
          return StringDyn.concat(tokenURI, StringDyn(tokenId));
        });
      })
      .while(true)
      .invariant(balance() == 0)
      .define(() => {
        const approve = (to, tokenId) => {
          const owner = ownerOf(tokenId);
          tokenApprovals[tokenId] = to;
          E.Approval(owner, to, tokenId);
        }
        const transferChecks = (caller, from_, to, tokenId) => {
          const owner = ownerOf(tokenId);
          check(owner == from_, "ERC721::transfer: transfer from incorrect owner");
          check(to != zeroAddr, "ERC721::transfer: transfer to the zero address");
          check(isApprovedOrOwner(caller, tokenId), "ERC721::transfer: caller is not owner nor approved");
        }
        const transfer_ = (caller, from_, to, tokenId) => {
          transferChecks(caller, from_, to, tokenId);
          approve(zeroAddr, tokenId);
          balances[from_] = maybe(balances[from_], 0, (b) => b - 1);
          balances[to]    = maybe(balances[to]   , 1, (b) => b + 1);
          owners[tokenId] = to;
          E.Transfer(from_, to, tokenId);
        }
        const doSafeTransferFrom = (caller, from_, to, tokenId, data) => {
          transfer_(caller, from_, to, tokenId);
          const ctcMaybe = Contract.fromAddress(to);
          ctcMaybe.match({
            Some: (ctc) => {
              const r = remote(ctc, ERC721TokenReceiverI);
              const mv = r.onERC721Received(getAddress(), from_, tokenId, data);
              // This hex string is bytes4(keccak256("onERC721Received(address,address,uint256,bytes)"))
              enforce(mv == Bytes.fromHex('0x150b7a02'));
            },
            None: () => {},
          });
        }
      })
      .api_(I.safeTransferFrom1, (from_, to, tokenId, data) => {
        transferChecks(this, from_, to, tokenId);
        return [ (k) => {
          doSafeTransferFrom(this, from_, to, tokenId, data);
          k(null);
          return [ ];
        }];
      })
      .api_(I.safeTransferFrom2, (from_, to, tokenId) => {
        transferChecks(this, from_, to, tokenId);
        return [ (k) => {
          doSafeTransferFrom(this, from_, to, tokenId, emptyBytesDyn);
          k(null);
          return [ ];
        }];
      })
      .api_(I.transferFrom, (from_, to, tokenId) => {
        transferChecks(this, from_, to, tokenId);
        return [ (k) => {
          transfer_(this, from_, to, tokenId);
          k(null);
          return [ ];
        }];
      })
      .api_(I.approve, (to, tokenId) => {
        const owner = ownerOf(tokenId);
        check(to != owner, "ERC721::approve: Approval to current owner");
        check(this == owner || isApprovedForAll(owner, this), "ERC721::approve: Caller is not owner nor approved for all");
        return [ (k) => {
          approve(to, tokenId);
          k(null);
          return [ ];
        }];
      })
      .api_(I.setApprovalForAll, (operator, approved) => {
        const owner = this;
        check(owner != operator, "ERC721::setApprovalForAll: approve to caller");
        return [ (k) => {
          operatorApprovals[[owner, operator]] = approved;
          E.ApprovalForAll(owner, operator, approved);
          k(null);
          return [ ];
        }];
      })
      .api_(I.mint, (to, tokenId) => {
        check(to != zeroAddr, "Cannot mint to zero address");
        check(!tokenExists(tokenId), "Token already exists");
        check(this == D, "mint can only be called by deployer")
        return [ (k) => {
          balances[to] = fromMaybe(balances[to], () => 1, (x) => x + 1);
          owners[tokenId] = to;
          E.Transfer(zeroAddr, to, tokenId);
          k(null);
          return [ ];
        }];
      })
      .api_(I.burn, (tokenId) => {
        const owner = ownerOf(tokenId);
        check(isApprovedOrOwner(this, tokenId), "ERC721::burn: caller is not owner nor approved");
        return [ (k) => {
          approve(zeroAddr, tokenId);
          const curBal = balances[owner];
          const newBal = fromMaybe(curBal, () => 0, (x) => x-1);
          if (newBal == 0) {
            delete balances[owner];
          } else {
            balances[owner] = newBal;
          }
          delete owners[tokenId];
          E.Transfer(owner, zeroAddr, tokenId);
          k(null);
          return [ ];
        }];
      });
  commit();
  exit();
});

export const testTokenReceiver = Reach.App(() => {
  setOptions({ connectors: [ETH] });
  const A = API({
    ...ERC721TokenReceiverI,
    transfer: Fun([Address, Address, UInt, BytesDyn], Null),
  });
  const D = Participant('Deployer', {
    deployed: Fun([Contract], Null),
  });
  init();

  D.publish();
  const inventory = new Map(Tuple(Address, UInt), Null);
  D.interact.deployed(getContract());

  const implIface = {
    ownerOf: Fun([UInt], Address),
    safeTransferFrom: Fun([Address, Address, UInt, BytesDyn], Null),
  };

  const [] = parallelReduce([])
        .while(true)
        .invariant(balance() == 0)
        .api_(A.onERC721Received, (ercCtcAddr, from_, tokenId, data) => {
          // I think this API should only be called by the implementing contract...
          check(this == ercCtcAddr);
          return [ (k) => {
            const ercCtcM = Contract.fromAddress(ercCtcAddr);
            ercCtcM.match({
              Some: (ercCtc) => {
                const impl = remote(ercCtc, implIface);
                const owner = impl.ownerOf(tokenId);
                enforce(owner == getAddress(), "onERC721Received: must have been set as owner of token");
                inventory[[ercCtcAddr, tokenId]] = null;
              },
              None: () => {
                enforce(false, "onERC721Received called with a bad contract address")
              },
            });
            k(Bytes.fromHex('0x150b7a02'));
            return [ ];
          }];
        })
        .api_(A.transfer, (ercCtcAddr, to, tokenId, data) => {
          // A serious contract for receiving NFTs should probably either include a manual override besides onERC721Received to check to add tokens to its inventory, or should dynamically check when transferring like this.  IE maybe I should remove the inventory and just do dynamic checks when transferring?
          check(isSome(inventory[[ercCtcAddr, tokenId]]));
          return [ (k) => {
            const ercCtcM = Contract.fromAddress(ercCtcAddr);
            ercCtcM.match({
              Some: (ercCtc) => {
                const impl = remote(ercCtc, implIface);
                delete inventory[[ercCtcAddr, tokenId]];
                impl.safeTransferFrom(getAddress(), to, tokenId, data);
              },
              None: () => {
                enforce(false, "transfer called with a bad contract address")
              },
            });
            k(null);
            return [ ];
          }];
        })
  commit();
  exit();
});
