pragma abicoder v2;

pragma solidity ^0.8.0;

contract WeirdContract {
  uint256 _x;
  uint256 setCount;
  uint256 getCount;
  uint256 amt;

  constructor() payable {
    _x = 0;
    setCount = 0;
    getCount = 0;
  }

  function setX(uint256 x) external payable {
    setCount++;
    _x = x;
    if ( setCount == 2 ) {
      amt = msg.value;
    }
  }

  function getX() external payable returns (uint256 x) {
    getCount++;
    x = _x;
    if ( getCount == 2 ) {
      uint256 b = amt / 2;
      payable(msg.sender).transfer(b);
      amt = amt - b;
    } else if ( getCount == 3 ) {
      payable(msg.sender).transfer(amt);
    }
  }
}
