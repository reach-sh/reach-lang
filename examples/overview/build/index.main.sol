// Automatically generated with Reach 0.1.2
pragma abicoder v2;

pragma solidity ^0.8.2;
interface IERC20 {
    function allowance(address owner, address spender) external view returns (uint256);
    function transferFrom(address sender, address recipient, uint256 amount) external returns (bool);
    function transfer(address recipient, uint256 amount) external returns (bool);
    function approve(address spender, uint256 amount) external returns (bool);
    function balanceOf(address owner) external view returns (uint256);
}

contract Stdlib {
  function safeAdd(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require((z = x + y) >= x, "add overflow"); }
  function safeSub(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require((z = x - y) <= x, "sub wraparound"); }
  function safeMul(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require(y == 0 || (z = x * y) / y == x, "mul overflow"); }

  function unsafeAdd(uint256 x, uint256 y) internal pure returns (uint256 z) {
    unchecked { z = x + y; } }
  function unsafeSub(uint256 x, uint256 y) internal pure returns (uint256 z) {
    unchecked { z = x - y; } }
  function unsafeMul(uint256 x, uint256 y) internal pure returns (uint256 z) {
    unchecked { z = x * y; } }

  function checkFunReturn(bool succ, bytes memory returnData, string memory errMsg) internal pure returns (bytes memory) {
    if (succ) {
      return returnData;
    } else {
      if (returnData.length > 0) {
        assembly {
          let returnData_size := mload(returnData)
          revert(add(32, returnData), returnData_size)
        }
      } else {
        revert(errMsg);
      }
    }
  }

  function tokenAllowance(address payable token, address owner, address spender) internal returns (uint256 amt) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.allowance.selector, owner, spender));
    checkFunReturn(ok, ret, 'token.allowance');
    amt = abi.decode(ret, (uint256));
  }

  function tokenTransferFrom(address payable token, address sender, address recipient, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.transferFrom.selector, sender, recipient, amt));
    checkFunReturn(ok, ret, 'token.transferFrom');
    res = abi.decode(ret, (bool));
  }

  function tokenTransfer(address payable token, address recipient, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.transfer.selector, recipient, amt));
    checkFunReturn(ok, ret, 'token.transfer');
    res = abi.decode(ret, (bool));
  }

  function safeTokenTransfer(address payable token, address recipient, uint256 amt) internal {
    require(tokenTransfer(token, recipient, amt));
  }

  function readPayAmt(address sender, address payable token) internal returns (uint256 amt) {
    amt = tokenAllowance(token, sender, address(this));
    require(checkPayAmt(sender, token, amt));
  }

  function checkPayAmt(address sender, address payable token, uint256 amt) internal returns (bool) {
    return tokenTransferFrom(token, sender, address(this), amt);
  }

  function tokenApprove(address payable token, address spender, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.approve.selector, spender, amt));
    checkFunReturn(ok, ret, 'token.approve');
    res = abi.decode(ret, (bool));
  }

  function tokenBalanceOf(address payable token, address owner) internal returns (uint256 res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0) }(abi.encodeWithSelector(IERC20.balanceOf.selector, owner));
    checkFunReturn(ok, ret, 'token.balanceOf');
    res = abi.decode(ret, (uint256));
  }
}

struct T0 {
  uint256 v18;
  }
struct T1 {
  address payable v24;
  uint256 v25;
  uint256 v27;
  }
struct T2 {
  uint256 v25;
  }
struct T3 {
  T0 svs;
  T2 msg;
  }
struct T4 {
  address payable v24;
  uint256 v25;
  uint256 v34;
  }
struct T6 {
  T1 svs;
  bool msg;
  }
struct T7 {
  uint8[128] v39;
  }
struct T8 {
  T4 svs;
  T7 msg;
  }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  event e0();
  struct _F0 {
    uint256 v18;
    }
  constructor() payable {
    emit e0();
    _F0 memory _f;
    _f.v18 = uint256(block.number);
    
    
    T0 memory nsvs;
    nsvs.v18 = _f.v18;
    current_state = uint256(keccak256(abi.encode(uint256(0), nsvs)));
    
    }
  
  
  
  event e1(T3 _a);
  
  function m1(T3 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a.svs))));
    current_state = 0x0;
    
    
    
    require(msg.value == uint256(0));
    emit e1(_a);
    T1 memory nsvs;
    nsvs.v24 = payable(msg.sender);
    nsvs.v25 = _a.msg.v25;
    nsvs.v27 = uint256(block.number);
    current_state = uint256(keccak256(abi.encode(uint256(1), nsvs)));
    
    }
  
  event e2(T6 _a);
  
  function m2(T6 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a.svs))));
    current_state = 0x0;
    
    
    
    require(msg.value == _a.svs.v25);
    emit e2(_a);
    T4 memory nsvs;
    nsvs.v24 = _a.svs.v24;
    nsvs.v25 = _a.svs.v25;
    nsvs.v34 = uint256(block.number);
    current_state = uint256(keccak256(abi.encode(uint256(2), nsvs)));
    
    }
  
  event e3(T8 _a);
  
  function m3(T8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _a.svs))));
    current_state = 0x0;
    
    
    
    require(msg.value == uint256(0));
    require((_a.svs.v24 == payable(msg.sender)));
    _a.svs.v24.transfer(_a.svs.v25);
    emit e3(_a);
    current_state = 0x0;
    selfdestruct(payable(msg.sender));
    
    }
  
  
  receive () external payable {}
  }
