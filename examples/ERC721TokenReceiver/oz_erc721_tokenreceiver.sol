// SPDX-License-Identifier: Apache-2.0
pragma solidity ^0.8.0;

import "openzeppelin-contracts-4.7.3/contracts/token/ERC721/IERC721Receiver.sol";

contract OZ_ERC721_TokenReceiver is IERC721Receiver {
    event GotAToken(address operator, address from, uint256 tokenId, bytes data);

    function onERC721Received(
        address operator,
        address from,
        uint256 tokenId,
        bytes calldata data
    ) external returns (bytes4) {
        emit GotAToken(operator, from, tokenId, data);
        return IERC721Receiver.onERC721Received.selector;
    }
}
