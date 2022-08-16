/**
 * ERC-721 Specification: https://eips.ethereum.org/EIPS/eip-721
 */
'reach 0.1';

const defaultOptions = {
  metaNameLen: 256,
  metaSymbolLen: 256,
  metaTokenURILen: 256,
}

const make = ({ metaNameLen, metaSymbolLen, metaTokenURILen }) => Reach.App(() => {
  setOptions({ connectors: [ETH] });

  const D = Participant('Deployer', {
    meta: Object({
      // XXX these should all be `string` per spec
      name: Bytes(metaNameLen),
      symbol: Bytes(metaSymbolLen),
      tokenURI: Bytes(metaTokenURILen),
    }),
    enum: Object({
      totalSupply: UInt
    }),
    zeroAddr: Address,
    deployed: Fun([Contract], Null),
    log: Fun(true, Null),
  });

  const metaViews = {
    name: Bytes(metaNameLen),
    symbol: Bytes(metaSymbolLen),
    tokenURI: Fun([UInt], Bytes(metaTokenURILen))
  }

  const enumViews = {
    totalSupply: UInt,
  }

  const V = View({
    balanceOf: Fun([Address], UInt),
    getApproved: Fun([UInt], Address),
    isApprovedForAll: Fun([Address, Address], Bool),
    ownerOf: Fun([UInt], Address),
    supportsInterface: Fun([Bytes(4)], Bool),
    ...metaViews,
    ...enumViews,
  });

  const I = API({
    approve: Fun([Address, UInt], Null),
    safeTransferFrom: Fun([Address, Address, UInt], Null),
    setApprovalForAll: Fun([Address, Bool], Null),
    transferFrom: Fun([Address, Address, UInt], Null),
    mint: Fun([Address, UInt], Null),
    burn: Fun([UInt], Null),
  });

  const E = Events({
    Approval: [Address, Address, UInt],
    ApprovalForAll: [Address, Address, Bool],
    Transfer: [Address, Address, UInt]
  });

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
  V.supportsInterface.set((id) => true);

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
          check(isSome(m_bal), "ERC721::balanceOf: No balance for address");
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
          // XXX tokenURI <> tokenId.toString()
          return tokenURI;
        });

      })
      .while(true)
      .invariant(true)
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
          balances[to]    = maybe(balances[to]   , 0, (b) => b + 1);
          owners[tokenId] = to;
          E.Transfer(from_, to, tokenId);
        }
      })
      .api_(I.safeTransferFrom, (from_, to, tokenId) => {
        check(isApprovedOrOwner(this, tokenId), "ERC721::safeTransferFrom: transfer caller is not owner nor approved");
        transferChecks(from_, to, tokenId);
        return [ (k) => {
          transfer_(from_, to, tokenId);
          // XXX this is where you'd send the arbitrary data to the `to` contract
          k(null);
          return [ ];
        }];
      })
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
        return [ (k) => {
          modAt(balances, to, (x) => x + 1);
          owners[tokenId] = to;
          E.Transfer(zeroAddr, to, tokenId);
          k(null);
          return [ ];
        }];
      })
      .api_(I.burn, (tokenId) => {
        const owner = ownerOf(tokenId);
        return [ (k) => {
          approve(zeroAddr, tokenId);
          modAt(balances, owner, (x) => x - 1);
          delete owners[tokenId];
          E.Transfer(owner, zeroAddr, tokenId);
          k(null);
          return [ ];
        }];
      })
      .timeout(false);

  commit();

});

export const main = make({ ...defaultOptions });
