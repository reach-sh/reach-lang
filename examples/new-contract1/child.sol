pragma abicoder v2;

pragma solidity ^0.8.0;

contract Contract {
  uint256 _x;
  uint256 _y;

  constructor(uint256 y) payable {
    _x = 0;
    _y = y;
  }

  function f(uint256 z) external payable returns (uint256 w) {
    w = _x + _y + z;
  }
}

