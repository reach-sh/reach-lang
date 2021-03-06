pragma abicoder v2;

pragma solidity ^0.8.0;

struct posn {
  uint256 x;
  uint256 y;
}

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

  function setX(posn memory p) external payable {
    setCount++;
    _x = p.x;
    if ( setCount == 2 ) {
      amt = msg.value;
    }
  }

  function getX() external payable returns (posn memory p) {
    p.y = getCount;
    getCount++;
    p.x = _x;
    if ( getCount == 2 ) {
      uint256 b = amt / 2;
      payable(msg.sender).transfer(b);
      amt = amt - b;
    } else if ( getCount == 3 ) {
      payable(msg.sender).transfer(amt);
    }
  }
}
