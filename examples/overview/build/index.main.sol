// Automatically generated with Reach 0.1.2
pragma abicoder v2;

pragma solidity ^0.8.0;
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
}

struct T0 {
  uint256 v2;
   }
struct T1 {
  address payable v5;
  uint256 v6;
  uint256 v11;
   }
struct T2 {
  uint256 v6;
   }
struct T3 {
  T0 svs;
  T2 msg;
   }
struct T4 {
  address payable v5;
  uint256 v6;
  uint256 v19;
   }
struct T6 {
  T1 svs;
  bool msg;
   }
struct T7 {
  uint8[128] v23;
   }
struct T8 {
  T4 svs;
  T7 msg;
   }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  event e0();
  struct _F0 {
    uint256 v2;
     }
  constructor() payable {
    emit e0();
    _F0 memory _f;
    _f.v2 = uint256(block.number);
    
    
    T0 memory nsvs;
    nsvs.v2 = _f.v2;
    current_state = uint256(keccak256(abi.encode(uint256(0), nsvs)));
    
     }
  
  
  
  event e1(T3 _a);
  
  function m1(T3 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a.svs))), 'state check at ./index.rsh:13:9:dot');
    current_state = 0x0;
    
    
    require(true && true, 'timeout check at ./index.rsh:13:9:dot');
    require((msg.value == uint256(0)), '(./index.rsh:13:9:dot,[],Just "pay amount correct")');
    require(true, '(./index.rsh:13:9:dot,[],Just "sender correct")');
    emit e1(_a);
    T1 memory nsvs;
    nsvs.v5 = payable(msg.sender);
    nsvs.v6 = _a.msg.v6;
    nsvs.v11 = uint256(block.number);
    current_state = uint256(keccak256(abi.encode(uint256(1), nsvs)));
    
     }
  
  event e2(T6 _a);
  
  function m2(T6 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a.svs))), 'state check at ./index.rsh:18:9:dot');
    current_state = 0x0;
    
    
    require(true && true, 'timeout check at ./index.rsh:18:9:dot');
    require((msg.value == _a.svs.v6), '(./index.rsh:18:9:dot,[],Just "pay amount correct")');
    require(true, '(./index.rsh:18:9:dot,[],Just "sender correct")');
    emit e2(_a);
    T4 memory nsvs;
    nsvs.v5 = _a.svs.v5;
    nsvs.v6 = _a.svs.v6;
    nsvs.v19 = uint256(block.number);
    current_state = uint256(keccak256(abi.encode(uint256(2), nsvs)));
    
     }
  
  event e3(T8 _a);
  
  function m3(T8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _a.svs))), 'state check at ./index.rsh:23:9:dot');
    current_state = 0x0;
    
    
    require(true && true, 'timeout check at ./index.rsh:23:9:dot');
    require((msg.value == uint256(0)), '(./index.rsh:23:9:dot,[],Just "pay amount correct")');
    require((_a.svs.v5 == payable(msg.sender)), '(./index.rsh:23:9:dot,[],Just "sender correct")');
    _a.svs.v5.transfer(_a.svs.v6);
    emit e3(_a);
    current_state = 0x0;
    selfdestruct(payable(msg.sender));
    
     }
  
  
  receive () external payable {}
   }
