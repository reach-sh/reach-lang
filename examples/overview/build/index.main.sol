// Automatically generated with Reach 0.1.2
pragma experimental ABIEncoderV2;


pragma solidity ^0.7.1;

contract Stdlib {
  function safeAdd(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require((z = x + y) >= x, "add overflow"); }
  function safeSub(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require((z = x - y) <= x, "sub wraparound"); }
  function safeMul(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require(y == 0 || (z = x * y) / y == x, "mul overflow"); }
}


contract ReachContract is Stdlib {
  uint256 current_state;
  
  event e0();
  constructor() payable {
    emit e0();
    
    a0postsvs memory nsvs;
    nsvs._last = uint256(block.number);
    current_state = uint256(keccak256(abi.encode(uint256(0), nsvs)));
    
     }
  
  
  
  
  
  
  
  
  
  
  struct a2postsvs {
    uint256 _last;
    uint256 v3;
    address payable v2;
     }
  
  struct a1postsvs {
    uint256 _last;
    uint256 v3;
    address payable v2;
     }
  
  struct a0postsvs {
    uint256 _last;
     }
  
  
  struct a1msg {
    uint256 v3;
     }
  struct a1 {
    a0postsvs svs;
    a1msg msg;
     }
  event e1(a1 _a);
  
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a.svs))));
    
    
    require(true && true);
    require(true);
    require((msg.value == uint256(0)));
    emit e1(_a);
    a1postsvs memory nsvs;
    nsvs._last = uint256(block.number);
    nsvs.v3 = _a.msg.v3;
    nsvs.v2 = msg.sender;
    current_state = uint256(keccak256(abi.encode(uint256(1), nsvs)));
    
     }
  
  
  struct a2 {
    a1postsvs svs;
     }
  event e2(a2 _a);
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a.svs))));
    
    
    require(true && true);
    require(true);
    require((msg.value == _a.svs.v3));
    emit e2(_a);
    a2postsvs memory nsvs;
    nsvs._last = uint256(block.number);
    nsvs.v3 = _a.svs.v3;
    nsvs.v2 = _a.svs.v2;
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
    require((_a.svs.v2 == msg.sender));
    require((msg.value == uint256(0)));
    _a.svs.v2.transfer(_a.svs.v3);
    emit e3(_a);
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
   }
