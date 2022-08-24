// SPDX-License-Identifier: Apache-2.0
pragma solidity ^0.8.0;

import "openzeppelin-contracts-4.7.3/contracts/token/ERC721/ERC721.sol";

contract OZ_ERC721 is ERC721 {
    constructor() ERC721("OZ_ERC721", "OZ") {}

    function mint(address to, uint256 tokenId, bytes calldata data) external {
        super._safeMint(to, tokenId, data);
    }
}
