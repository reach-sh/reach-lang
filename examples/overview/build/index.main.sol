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
    
    
    a0postsvs memory nsvs;
    nsvs.v2 = _f.v2;
    current_state = uint256(keccak256(abi.encode(uint256(0), nsvs)));
    
     }
  
  
  
  
  
  
  
  
  
  
  struct a2postsvs {
    address payable v5;
    uint256 v6;
    uint256 v19;
     }
  
  struct a1postsvs {
    address payable v5;
    uint256 v6;
    uint256 v11;
     }
  
  struct a0postsvs {
    uint256 v2;
     }
  
  
  struct a1msg {
    uint256 v6;
     }
  struct a1 {
    a0postsvs svs;
    a1msg msg;
     }
  event e1(a1 _a);
  
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a.svs))));
    
    
    require(true && true);
    
    require((msg.value == uint256(0)));
    require(true);
    emit e1(_a);
    a1postsvs memory nsvs;
    nsvs.v5 = payable(msg.sender);
    nsvs.v6 = _a.msg.v6;
    nsvs.v11 = uint256(block.number);
    current_state = uint256(keccak256(abi.encode(uint256(1), nsvs)));
    
     }
  
  
  struct a2 {
    a1postsvs svs;
     }
  event e2(a2 _a);
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a.svs))));
    
    
    require(true && true);
    
    require((msg.value == _a.svs.v6));
    require(true);
    emit e2(_a);
    a2postsvs memory nsvs;
    nsvs.v5 = _a.svs.v5;
    nsvs.v6 = _a.svs.v6;
    nsvs.v19 = uint256(block.number);
    current_state = uint256(keccak256(abi.encode(uint256(2), nsvs)));
    
     }
  
  
  struct a3msg {
    uint8[128] v23;
     }
  struct a3 {
    a2postsvs svs;
    a3msg msg;
     }
  event e3(a3 _a);
  
  function m3(a3 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _a.svs))));
    
    
    require(true && true);
    
    require((msg.value == uint256(0)));
    
    require((_a.svs.v5 == payable(msg.sender)));
    _a.svs.v5.transfer(_a.svs.v6);
    emit e3(_a);
    current_state = 0x0;
    selfdestruct(payable(msg.sender));
    
     }
  
  
   }
