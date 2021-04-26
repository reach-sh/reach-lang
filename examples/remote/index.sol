pragma abicoder v2;

pragma solidity ^0.8.0;

interface IERC20 {
    function allowance(address owner, address spender) external view returns (uint256);
    function transferFrom(address sender, address recipient, uint256 amount) external returns (bool);
    function transfer(address recipient, uint256 amount) external returns (bool);
    function _approve(address spender, address value) external returns (bool);
    function balanceOf(address owner) external view returns (uint256);
}

struct posn {
  uint256 x;
  uint256 y;
}

contract WeirdContract {
  uint256 _x;
  uint256 setCount;
  uint256 getCount;
  uint256 amt;

  // Test non-network token payments
  function tokenTransferFrom(address payable token, address sender, address recipient, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.transferFrom.selector, sender, recipient, amt));
    res = abi.decode(ret, (bool));
  }

  function tokenTransfer(address payable token, address recipient, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.transfer.selector, recipient, amt));
    res = abi.decode(ret, (bool));
  }

  function tokenAllowance(address payable token, address owner, address spender) internal returns (uint256 amt) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.allowance.selector, owner, spender));
    amt = abi.decode(ret, (uint256));
  }

  uint256 myBalance;
  //


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

  function payFn(address payable token) external payable {
    uint256 allowance = tokenAllowance(token, msg.sender, address(this));
    require(allowance >= 0, "payFn has no allowance");
    tokenTransferFrom(token, msg.sender, address(this), allowance);
    myBalance = allowance;
  }

  function bilFn(address payable token) external {
    tokenTransfer(token, msg.sender, myBalance);
  }
}