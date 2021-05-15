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
  uint256 v50;
  }
struct T1 {
  address payable v54;
  uint256 v55;
  uint256 v58;
  }
struct T2 {
  uint256 v55;
  }
struct T3 {
  T0 svs;
  T2 msg;
  }
struct T4 {
  address payable v54;
  uint256 v55;
  address payable v62;
  }
struct T5 {
  uint256 v66;
  uint256 v215;
  uint256 v217;
  }
struct T6 {
  T4 svs;
  T5 msg;
  }
struct T8 {
  T1 svs;
  bool msg;
  }
struct T9 {
  address payable v54;
  uint256 v55;
  address payable v62;
  uint256 v215;
  uint256 v217;
  }
struct T10 {
  address payable v54;
  uint256 v55;
  address payable v62;
  uint256 v66;
  }
struct T11 {
  T10 svs;
  bool msg;
  }
struct T12 {
  address payable v54;
  uint256 v55;
  address payable v62;
  uint256 v90;
  uint256 v93;
  uint256 v217;
  }
struct T13 {
  uint256 v90;
  }
struct T14 {
  T9 svs;
  T13 msg;
  }
struct T15 {
  T9 svs;
  bool msg;
  }
struct T16 {
  address payable v54;
  uint256 v55;
  address payable v62;
  uint256 v90;
  uint256 v99;
  uint256 v102;
  uint256 v217;
  }
struct T17 {
  uint256 v99;
  }
struct T18 {
  T12 svs;
  T17 msg;
  }
struct T19 {
  T12 svs;
  bool msg;
  }
struct T20 {
  uint256 v107;
  uint256 v108;
  }
struct T21 {
  T16 svs;
  T20 msg;
  }
struct T22 {
  T16 svs;
  bool msg;
  }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  event e0();
  struct _F0 {
    uint256 v50;
    }
  constructor() payable {
    emit e0();
    _F0 memory _f;
    _f.v50 = uint256(block.number);
    
    
    T0 memory nsvs;
    nsvs.v50 = _f.v50;
    current_state = uint256(keccak256(abi.encode(uint256(0), nsvs)));
    
    }
  
  
  
  event e1(T3 _a);
  
  function m1(T3 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a.svs))));
    current_state = 0x0;
    
    
    
    require(msg.value == _a.msg.v55);
    emit e1(_a);
    T1 memory nsvs;
    nsvs.v54 = payable(msg.sender);
    nsvs.v55 = _a.msg.v55;
    nsvs.v58 = uint256(block.number);
    current_state = uint256(keccak256(abi.encode(uint256(1), nsvs)));
    
    }
  
  event e2(T8 _a);
  
  function m2(T8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a.svs))));
    current_state = 0x0;
    
    
    require(uint256(block.number) < _a.svs.v58 + uint256(10));
    require(msg.value == _a.svs.v55);
    emit e2(_a);
    T6 memory la;
    la.svs.v54 = _a.svs.v54;
    la.svs.v55 = _a.svs.v55;
    la.svs.v62 = payable(msg.sender);
    la.msg.v66 = uint256(1);
    la.msg.v215 = uint256(block.number);
    la.msg.v217 = (_a.svs.v55 + _a.svs.v55);
    l4(la);
    
    }
  
  event e3(T8 _a);
  
  function m3(T8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a.svs))));
    current_state = 0x0;
    
    
    require(uint256(block.number) >= _a.svs.v58 + uint256(10));
    require(msg.value == uint256(0));
    require((_a.svs.v54 == payable(msg.sender)));
    _a.svs.v54.transfer(_a.svs.v55);
    emit e3(_a);
    current_state = 0x0;
    selfdestruct(payable(msg.sender));
    
    }
  
  
  function l4(T6 memory _a)  internal {
    
    if ((_a.msg.v66 == uint256(1))) {
      
      T9 memory nsvs;
      nsvs.v54 = _a.svs.v54;
      nsvs.v55 = _a.svs.v55;
      nsvs.v62 = _a.svs.v62;
      nsvs.v215 = _a.msg.v215;
      nsvs.v217 = _a.msg.v217;
      current_state = uint256(keccak256(abi.encode(uint256(4), nsvs)));
      }
    else {
      
      T11 memory la;
      la.svs.v54 = _a.svs.v54;
      la.svs.v55 = _a.svs.v55;
      la.svs.v62 = _a.svs.v62;
      la.svs.v66 = _a.msg.v66;
      l5(la);
      }
    }
  
  
  function l5(T11 memory _a)  internal {
    
    ((_a.svs.v66 == uint256(2)) ? _a.svs.v54 : _a.svs.v62).transfer((uint256(2) * _a.svs.v55));
    
    current_state = 0x0;
    selfdestruct(payable(msg.sender));
    
    }
  
  event e6(T14 _a);
  
  function m6(T14 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a.svs))));
    current_state = 0x0;
    
    
    require(uint256(block.number) < _a.svs.v215 + uint256(10));
    require(msg.value == uint256(0));
    require((_a.svs.v54 == payable(msg.sender)));
    emit e6(_a);
    T12 memory nsvs;
    nsvs.v54 = _a.svs.v54;
    nsvs.v55 = _a.svs.v55;
    nsvs.v62 = _a.svs.v62;
    nsvs.v90 = _a.msg.v90;
    nsvs.v93 = uint256(block.number);
    nsvs.v217 = _a.svs.v217;
    current_state = uint256(keccak256(abi.encode(uint256(6), nsvs)));
    
    }
  
  event e7(T15 _a);
  
  function m7(T15 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a.svs))));
    current_state = 0x0;
    
    
    require(uint256(block.number) >= _a.svs.v215 + uint256(10));
    require(msg.value == uint256(0));
    require((_a.svs.v62 == payable(msg.sender)));
    _a.svs.v62.transfer(_a.svs.v217);
    emit e7(_a);
    current_state = 0x0;
    selfdestruct(payable(msg.sender));
    
    }
  
  event e8(T18 _a);
  
  function m8(T18 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(6), _a.svs))));
    current_state = 0x0;
    
    
    require(uint256(block.number) < _a.svs.v93 + uint256(10));
    require(msg.value == uint256(0));
    require((_a.svs.v62 == payable(msg.sender)));
    emit e8(_a);
    T16 memory nsvs;
    nsvs.v54 = _a.svs.v54;
    nsvs.v55 = _a.svs.v55;
    nsvs.v62 = _a.svs.v62;
    nsvs.v90 = _a.svs.v90;
    nsvs.v99 = _a.msg.v99;
    nsvs.v102 = uint256(block.number);
    nsvs.v217 = _a.svs.v217;
    current_state = uint256(keccak256(abi.encode(uint256(8), nsvs)));
    
    }
  
  event e9(T19 _a);
  
  function m9(T19 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(6), _a.svs))));
    current_state = 0x0;
    
    
    require(uint256(block.number) >= _a.svs.v93 + uint256(10));
    require(msg.value == uint256(0));
    require((_a.svs.v54 == payable(msg.sender)));
    _a.svs.v54.transfer(_a.svs.v217);
    emit e9(_a);
    current_state = 0x0;
    selfdestruct(payable(msg.sender));
    
    }
  
  event e10(T21 _a);
  
  function m10(T21 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(8), _a.svs))));
    current_state = 0x0;
    
    
    require(uint256(block.number) < _a.svs.v102 + uint256(10));
    require(msg.value == uint256(0));
    require((_a.svs.v54 == payable(msg.sender)));
    require((_a.svs.v90 == (uint256(keccak256(abi.encode(_a.msg.v107, _a.msg.v108))))));
    emit e10(_a);
    T6 memory la;
    la.svs.v54 = _a.svs.v54;
    la.svs.v55 = _a.svs.v55;
    la.svs.v62 = _a.svs.v62;
    la.msg.v66 = ((_a.msg.v108 + (uint256(4) - _a.svs.v99)) % uint256(3));
    la.msg.v215 = uint256(block.number);
    la.msg.v217 = _a.svs.v217;
    l4(la);
    
    }
  
  event e11(T22 _a);
  
  function m11(T22 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(8), _a.svs))));
    current_state = 0x0;
    
    
    require(uint256(block.number) >= _a.svs.v102 + uint256(10));
    require(msg.value == uint256(0));
    require((_a.svs.v62 == payable(msg.sender)));
    _a.svs.v62.transfer(_a.svs.v217);
    emit e11(_a);
    current_state = 0x0;
    selfdestruct(payable(msg.sender));
    
    }
  
  
  receive () external payable {}
  }
