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

// TODO - undo this workaround when Bytes.fromHex is working, and turn all Bytes_fromHex back into Bytes.fromHex.  For now this needs to be 4 bytes long to fit the defined interface.
const Bytes_fromHex = (x) => "byte";

const ERC165 = mixin({
  IDs: [ Bytes_fromHex('0x01ffc9a7'), ],
  View: [{
    supportsInterface: Fun([Bytes(4)], Bool),
  }],
});

const ERC721 = mixin({
  Base: ERC165,
  IDs: [ Bytes_fromHex('0x80ac58cd'), ],
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
    //safeTransferFrom3: Fun([Address, Contract, UInt, BytesDyn], Null),
    transferFrom: Fun([Address, Address, UInt], Null),
    approve: Fun([Address, UInt], Null),
    setApprovalForAll: Fun([Address, Bool], Null),
  }, {
    safeTransferFrom1: 'safeTransferFrom',
    safeTransferFrom2: 'safeTransferFrom',
    //safeTransferFrom3: 'safeTransferFrom',
  }],
});

const ERC721Metadata = mixin({
  Base: ERC721,
  IDs: [ Bytes_fromHex('0x5b5e139f'), ],
  View: [{
    name: StringDyn,
    symbol: StringDyn,
    tokenURI: Fun([UInt], StringDyn),
  }],
});

const ERC721Enumerable = mixin({
  Base: ERC721,
  IDs: [ Bytes_fromHex('0x780e9d63'), ],
  View: [{
    totalSupply: UInt,
    tokenByIndex: Fun([UInt], UInt),
    tokenOfOwnerByIndex: Fun([Address, UInt], UInt),
  }],
});

const ERC721TokenReceiverI = {
    onERC721Received: Fun([Contract, Address, UInt, BytesDyn], Bytes(4)),
};
const ERC721TokenReceiver = mixin({
  IDs: [ Bytes_fromHex('0x150b7a02'), ],
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
  })
  D.publish(name, symbol, tokenURI, totalSupply, zeroAddr);

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

        const modAt = (dict, key, f) => {
          const value = dict[key];
          fromMaybe(value, () => {}, (x) => { dict[key] = f(x); });
        }

        V.balanceOf.set((owner) => {
          check(owner != zeroAddr, "ERC721::balanceOf: Address zero is not a valid owner");
          const m_bal = balances[owner];
          // TODO - I don't understand why we would want this check.  I think it's wrong, we want to return 0 if they own none.
          //check(isSome(m_bal), "ERC721::balanceOf: No balance for address");
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
        const transferChecks = (from_, to, tokenId) => {
          const owner = ownerOf(tokenId);
          check(owner == from_, "ERC721::transfer: transfer from incorrect owner");
          check(to != zeroAddr, "ERC721::transfer: transfer to the zero address");
        }
        const transfer_ = (from_, to, tokenId) => {
          transferChecks(from_, to, tokenId);
          approve(zeroAddr, tokenId);
          balances[from_] = maybe(balances[from_], 0, (b) => b - 1);
          balances[to]    = maybe(balances[to]   , 1, (b) => b + 1);
          owners[tokenId] = to;
          E.Transfer(from_, to, tokenId);
        }
      })
      .api_(I.safeTransferFrom1, (from_, to, tokenId, data) => {
        check(isApprovedOrOwner(this, tokenId), "ERC721::safeTransferFrom: transfer caller is not owner nor approved");
        transferChecks(from_, to, tokenId);
        return [ (k) => {
          transfer_(from_, to, tokenId);
          // TODO - this should be a remote call if and only if the to address is a contract.  We need to use the future feature Contract.fromAddress here to determine that.
          //const to_ctc = remote(to, ERC721TokenReceiverI);
          //const mv = to_ctc.onERC721Received(getContract(), from_, tokenId, data);
          //// This hex string is bytes4(keccak256("onERC721Received(address,address,uint256,bytes)"))
          //ensure(mv == Bytes_fromHex('0x150b7a02'));
          k(null);
          return [ ];
        }];
      })
      .api_(I.safeTransferFrom2, (from_, to, tokenId) => {
        // TODO - this check is important, but no transfers are working with it on.  So something is wrong with it (or with how I'm calling this method).
        //check(isApprovedOrOwner(this, tokenId), "ERC721::safeTransferFrom: transfer caller is not owner nor approved");
        transferChecks(from_, to, tokenId);
        return [ (k) => {
          transfer_(from_, to, tokenId);
          k(null);
          return [ ];
        }];
      })
      //.api_(I.safeTransferFrom3, (from_, to, tokenId, data) => {
      //  // TODO - temporary - "You should make safeTransferFrom3 which has a Contract rather than an Address or just have a Contract in safeTransferFrom1 and try to make progress with the current test suite" -- IE this is a duplicate of safeTransferFrom1, but that takes different types until we sort out conversion between Addresses and Contracts. 
      //  check(isApprovedOrOwner(this, tokenId), "ERC721::safeTransferFrom: transfer caller is not owner nor approved");
      //  transferChecks(from_, to, tokenId);
      //  return [ (k) => {
      //    transfer_(from_, to, tokenId);
      //    const to_ctc = remote(to, ERC721TokenReceiverI);
      //    const mv = to_ctc.onERC721Received(getContract(), from_, tokenId, data);
      //    ensure(mv == Bytes_fromHex('0x150b7a02'));
      //    k(null);
      //    return [ ];
      //  }];
      //})
      .api_(I.transferFrom, (from_, to, tokenId) => {
        check(isApprovedOrOwner(this, tokenId), "ERC721::transferFrom: transfer caller is not owner nor approved");
        transferChecks(from_, to, tokenId);
        return [ (k) => {
          transfer_(from_, to, tokenId);
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
        // TODO - shouldn't minting only be available to the deployer or something like that?
        return [ (k) => {
          balances[to] = fromMaybe(balances[to], () => 1, (x) => x + 1);
          owners[tokenId] = to;
          E.Transfer(zeroAddr, to, tokenId);
          k(null);
          return [ ];
        }];
      })
      .api_(I.burn, (tokenId) => {
        // TODO - I don't see any check here that only the owner can burn it...
        const owner = ownerOf(tokenId);
        return [ (k) => {
          approve(zeroAddr, tokenId);
          modAt(balances, owner, (x) => x - 1);
          delete owners[tokenId];
          E.Transfer(owner, zeroAddr, tokenId);
          k(null);
          return [ ];
        }];
      });
  commit();
  exit();
});
