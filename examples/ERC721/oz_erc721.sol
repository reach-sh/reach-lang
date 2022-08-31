// SPDX-License-Identifier: Apache-2.0
pragma solidity ^0.8.0;

import "openzeppelin-contracts-4.7.3/contracts/token/ERC721/extensions/ERC721Enumerable.sol";

contract OZ_ERC721 is ERC721Enumerable {
    constructor() ERC721("OZ_ERC721", "OZ") {}

    function mint(address to, uint256 tokenId) external {
        super._safeMint(to, tokenId);
    }
    
    function _baseURI() override internal pure returns (string memory) {
        return "OZ_ERC721/";
    }
}
