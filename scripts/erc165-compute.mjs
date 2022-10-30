#!/usr/bin/env node

import {ethers} from 'ethers';
const keccak256 = ethers.utils.keccak256;

const mkInterfaceIdentifier = (funcIds) => {

  const selectors = funcIds.map(x => {
    const hashed = keccak256(Buffer.from(x))
    const hashedBuf = Buffer.from(hashed.substring(2), "hex")
    return hashedBuf.slice(0,4)
  })
  const result = Array(4).fill(0);
  for (const s of selectors) {
    for (let i = 0; i < 4; i++) {
      result[i] ^= s[i];
    }
  }
  return "0x" + Buffer.from(result).toString("hex")
}


const ifaces = {
  ERC165: [
    "supportsInterface(bytes4)",
  ],
  ERC721: [
    "balanceOf(address)",
    "ownerOf(uint256)",
    "safeTransferFrom(address,address,uint256,bytes)",
    "safeTransferFrom(address,address,uint256)",
    "transferFrom(address,address,uint256)",
    "approve(address,uint256)",
    "setApprovalForAll(address,bool)",
    "getApproved(uint256)",
    "isApprovedForAll(address,address)",
  ],
  ERC721Enumerable: [
    "totalSupply()",
    "tokenOfOwnerByIndex(address,uint256)",
    "tokenByIndex(uint256)"
  ],
  ERC721TokenReceiver: [
    "onERC721Received(address,address,uint256,bytes)",
  ],
  ERC20: [
    "totalSupply()",
    "balanceOf(address)",
    "transfer(address,uint256)",
    "allowance(address,address)",
    "approve(address,uint256)",
    "transferFrom(address,address,uint256)"
  ],
}

for (const k of Object.keys(ifaces)) {
  console.log(`${k}: ${mkInterfaceIdentifier(ifaces[k])}`)
}
