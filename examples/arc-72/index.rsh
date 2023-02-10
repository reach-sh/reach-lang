'reach 0.1';
'use strict';

// baseUriLength is the length of ipfs://<v1-CID-in-base32-format>/
const baseUriLength = 67;
const metadataUriType = Bytes(256);
const NftId = UInt256;
// This NFT is limitted to 10^3 tokens for the sake of the `tokenUri` method to construct a URI.  But this limit could be raised by extending the URI construction.
const maxNftId = 9999;

export const main = Reach.App(() => {
  setOptions({
    connectors: [ALGO],
  });
  const D = Participant('Deployer', {
    ready: Fun([], Null),
    params: Object({
      zeroAddress: Address,
      metadataUriBase: Bytes(baseUriLength),
    }),
  });
  const A = API({
    transferFrom: Fun([Address, Address, NftId], Null),
    approve: Fun([Address, NftId], Null),
    setApprovalForAll: Fun([Address, Bool], Null),
    updateAdmin: Fun([Address], Null),
    mintTo: Fun([Address], NftId),
    burn: Fun([NftId], Null),
  });
  const V = View({
    ownerOf: Fun([NftId], Address),
    tokenURI: Fun([NftId], metadataUriType),
    supportsInterface: Fun([Bytes(4)], Bool),
    getApproved: Fun([NftId], Address),
    isApprovedForAll: Fun([Address, Address], Bool),
    totalSupply: Fun([], UInt256),
    currentAdmin: Fun([], Address),

  });
  const E = Events({
    Transfer: [Address, Address, NftId],
    Approval: [Address, Address, NftId],
    ApprovalForAll: [Address, Address, Bool],
  });

  init();
  D.only(() => {
    const params = declassify(interact.params);
  });
  D.publish(params);

  // nftData is [ownerAddress, approvedAddress]
  const nftData = new Map(NftId, Tuple(Address, Address));
  const getNft = (nftId, mustExist) => {
    if (mustExist) {
      check(isSome(nftData[nftId]), "nft must exist");
    }
    return fromSome(nftData[nftId], [params.zeroAddress, params.zeroAddress]);
  }
  const getNftOwner = (nftId, mustExist) => getNft(nftId, mustExist)[0];
  const getNftApproved = (nftId, mustExist) => getNft(nftId, mustExist)[1];

  const operatorData = new Map(Tuple(Address, Address), Bool);
  const getApprovalForAll = (owner, operator) => {
    return fromSome(operatorData[[owner, operator]], false);
  }

  const canTransfer = (nftId, addr) => {
    const [owner, controller] = getNft(nftId, true);
    return addr == owner || addr == controller || getApprovalForAll(owner, addr);
  }

  D.interact.ready();

  const vars = parallelReduce({
    adminAddress: D,
    nMinted: UInt256(0),
    totalSupply: UInt256(0),
  })
    .define(() => {

      // helpers
      const noPayment = () => [0];
      const tokenUri = (nftId) => {
        check(isSome(nftData[nftId]), "nft must exist");
        const idShort = UInt(nftId, true);
        const digitArr = array(Bytes(1),
                               ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9",]);
        const digit0 = digitArr[idShort % 10];
        const digit1 = digitArr[(idShort / 10) % 10];
        const digit2 = digitArr[(idShort / 100) % 10];
        const digit3 = digitArr[(idShort / 1000) % 10];
        const digits1 = Bytes.concat(digit1, digit0);
        const digits2 = Bytes.concat(digit2, digits1);
        const digits3 = Bytes.concat(digit3, digits2);
        const uri = Bytes.concat(params.metadataUriBase, digits3);
        return metadataUriType.pad(uri);
      }
      const ifaceSelectors = [
        // ARC-73 (supportsInterface)
        Bytes.fromHex("0x4e22a3ba"),
        // ARC-72 Core
        Bytes.fromHex("0x15974096"),
        // ARC-72 Metadata extension
        Bytes.fromHex("0x9112544c"),
        // ARC-72 Transfer Management extension
        Bytes.fromHex("0x924d64fb"),
      ];
      const supportsInterface = (ifaceSelector) => {
        return ifaceSelectors.includes(ifaceSelector);
      }

      //views
      V.ownerOf.set(nftId => getNftOwner(nftId, false));
      V.tokenURI.set(tokenUri);
      V.supportsInterface.set(supportsInterface);
      V.getApproved.set(nftId => getNftApproved(nftId, false));
      V.isApprovedForAll.set(getApprovalForAll);
      V.totalSupply.set(() => vars.totalSupply);
      V.currentAdmin.set(() => vars.adminAddress);
    })
    .invariant(balance() === 0)
    .while(true)
    .api_(A.updateAdmin, (newAdmin) => {
      check(vars.adminAddress == this, "must be admin")
      return [
        noPayment(),
        (k) => {
          k(null);
          return {...vars, adminAddress: newAdmin};
        },
      ];
    })
    .api_(A.mintTo, (firstOwner) => {
      check(vars.adminAddress == this, "must be admin");
      check(vars.nMinted <= UInt256(maxNftId), "already minted max NFT")
      const nftId = vars.nMinted + UInt256(1);
      const newTotalSupply = vars.totalSupply + UInt256(1);
      return [
        noPayment(),
        (k) => {
          nftData[nftId] = [firstOwner, params.zeroAddress];
          E.Transfer(params.zeroAddress, firstOwner, nftId);
          k(nftId);
          return {
            ...vars,
            nMinted: nftId,
            totalSupply: newTotalSupply,
          };
        },
      ];
    })
    .api_(A.approve, (controller, nftId) => {
      const owner = getNftOwner(nftId, true);
      check(this == owner, "must be nft owner");
      return [
        noPayment(),
        (k) => {
          nftData[nftId] = [owner, controller];
          E.Approval(owner, controller, nftId);
          k(null);
          return vars;
        },
      ];
    })
    .api_(A.setApprovalForAll, (operator, tOrF) => {
      return [
        noPayment(),
        (k) => {
          if (tOrF) {
            operatorData[[this, operator]] = true;
          } else {
            delete operatorData[[this, operator]];
          }
          E.ApprovalForAll(this, operator, tOrF);
          k(null);
          return vars;
        },
      ];
    })
    .api_(A.transferFrom, (oldOwner, newOwner, nftId) => {
      const oldOwnerReal = getNftOwner(nftId, true);
      check(oldOwnerReal == oldOwner, "owner specified in API must be correct");
      check(canTransfer(nftId, this), "must be nft owner or approved operator");
      return [
        noPayment(),
        (k) => {
          nftData[nftId] = [newOwner, params.zeroAddress];
          E.Transfer(oldOwner, newOwner, nftId);
          k(null);
          return vars;
        },
      ];
    })
    .api_(A.burn, (nftId) => {
      const owner = getNftOwner(nftId, true);
      check(canTransfer(nftId, this), "must be nft owner or approved operator");
      return [
        noPayment(),
        (k) => {
          delete nftData[nftId];
          E.Transfer(owner, params.zeroAddress, nftId);
          k(null);
          return {
            ...vars,
            totalSupply: vars.totalSupply - UInt256(1),
          };
        },
      ];
    })
  ;
  commit();
  exit();
});
