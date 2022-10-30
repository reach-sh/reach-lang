// SPDX-License-Identifier: Apache-2.0
pragma solidity ^0.8.0;

import "openzeppelin-contracts-4.7.3/contracts/token/ERC20/ERC20.sol";

contract OZ_ERC20 is ERC20 {
  uint8 _decimals;
  constructor(string memory name, string memory symbol, uint8 __decimals, uint256 __totalSupply) ERC20(name, symbol) {
    _mint(msg.sender, __totalSupply);
    _decimals = __decimals;
  }
  function decimals() public view override returns (uint8) {
    return _decimals;
  }
}
