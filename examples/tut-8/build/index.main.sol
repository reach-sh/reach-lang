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
  
  
  
  
  
  
  
  
  
  struct a5postsvs {
    uint256 _last;
    uint256 v48;
    address payable v47;
    address payable v82;
    uint256 v139;
    uint256 v175;
    uint256 v182;
     }
  
  struct a4postsvs {
    uint256 _last;
    uint256 v48;
    address payable v47;
    address payable v82;
    uint256 v139;
    uint256 v146;
     }
  
  struct a3postsvs {
    uint256 _last;
    uint256 v48;
    address payable v47;
    address payable v82;
    uint256 v90;
     }
  
  struct a1postsvs {
    uint256 _last;
    uint256 v48;
    uint256 v54;
    address payable v47;
     }
  
  struct a0postsvs {
    uint256 _last;
     }
  
  
  struct a1msg {
    uint256 v48;
     }
  struct a1 {
    a0postsvs svs;
    a1msg msg;
     }
  event e1(a1 _a);
  struct _F1 {
    uint256 v54;
     }
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a.svs))));
    _F1 memory _f;
    
    require(true && true);
    require(true);
    require((msg.value == _a.msg.v48));
    _f.v54 = safeAdd((uint256(0)), msg.value);
    emit e1(_a);
    a1postsvs memory nsvs;
    nsvs._last = uint256(block.number);
    nsvs.v48 = _a.msg.v48;
    nsvs.v54 = _f.v54;
    nsvs.v47 = msg.sender;
    current_state = uint256(keccak256(abi.encode(uint256(1), nsvs)));
    
     }
  
  
  struct a2 {
    a1postsvs svs;
     }
  event e2(a2 _a);
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a.svs))));
    
    
    require(true && uint256(block.number) < safeAdd(_a.svs._last, uint256(10)));
    require(true);
    require((msg.value == _a.svs.v48));
    emit e2(_a);
    a3 memory la;
    la.svs.v48 = _a.svs.v48;
    la.svs.v47 = _a.svs.v47;
    la.svs.v82 = msg.sender;
    la.msg.v90 = ((safeAdd((_a.svs.v54), msg.value)));
    la.msg.v91 = uint256(1);
    l3(la);
    
     }
  
  
  struct a3msg {
    uint256 v90;
    uint256 v91;
     }
  struct a3svs {
    uint256 v48;
    address payable v47;
    address payable v82;
     }
  struct a3 {
    a3svs svs;
    a3msg msg;
     }
  
  function l3(a3 memory _a)  internal {
    
    if ((_a.msg.v91 == uint256(1))) {
      
      a3postsvs memory nsvs;
      nsvs._last = uint256(block.number);
      nsvs.v48 = _a.svs.v48;
      nsvs.v47 = _a.svs.v47;
      nsvs.v82 = _a.svs.v82;
      nsvs.v90 = _a.msg.v90;
      current_state = uint256(keccak256(abi.encode(uint256(3), nsvs)));
       }
    else {
      ((_a.msg.v91 == uint256(2)) ? _a.svs.v47 : _a.svs.v82).transfer((safeMul(uint256(2), _a.svs.v48)));
      
      current_state = 0x0;
      selfdestruct(msg.sender);
       }
     }
  
  
  struct a4msg {
    uint256 v139;
     }
  struct a4 {
    a3postsvs svs;
    a4msg msg;
     }
  event e4(a4 _a);
  struct _F4 {
    uint256 v146;
     }
  function m4(a4 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a.svs))));
    _F4 memory _f;
    
    require(true && uint256(block.number) < safeAdd(_a.svs._last, uint256(10)));
    require((_a.svs.v47 == msg.sender));
    require((msg.value == uint256(0)));
    _f.v146 = safeAdd((_a.svs.v90), msg.value);
    emit e4(_a);
    a4postsvs memory nsvs;
    nsvs._last = uint256(block.number);
    nsvs.v48 = _a.svs.v48;
    nsvs.v47 = _a.svs.v47;
    nsvs.v82 = _a.svs.v82;
    nsvs.v139 = _a.msg.v139;
    nsvs.v146 = _f.v146;
    current_state = uint256(keccak256(abi.encode(uint256(4), nsvs)));
    
     }
  
  
  struct a5msg {
    uint256 v175;
     }
  struct a5 {
    a4postsvs svs;
    a5msg msg;
     }
  event e5(a5 _a);
  struct _F5 {
    uint256 v182;
     }
  function m5(a5 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a.svs))));
    _F5 memory _f;
    
    require(true && uint256(block.number) < safeAdd(_a.svs._last, uint256(10)));
    require((_a.svs.v82 == msg.sender));
    require((msg.value == uint256(0)));
    _f.v182 = safeAdd((_a.svs.v146), msg.value);
    emit e5(_a);
    a5postsvs memory nsvs;
    nsvs._last = uint256(block.number);
    nsvs.v48 = _a.svs.v48;
    nsvs.v47 = _a.svs.v47;
    nsvs.v82 = _a.svs.v82;
    nsvs.v139 = _a.svs.v139;
    nsvs.v175 = _a.msg.v175;
    nsvs.v182 = _f.v182;
    current_state = uint256(keccak256(abi.encode(uint256(5), nsvs)));
    
     }
  
  
  struct a6msg {
    uint256 v210;
    uint256 v211;
     }
  struct a6 {
    a5postsvs svs;
    a6msg msg;
     }
  event e6(a6 _a);
  
  function m6(a6 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a.svs))));
    
    
    require(true && uint256(block.number) < safeAdd(_a.svs._last, uint256(10)));
    require((_a.svs.v47 == msg.sender));
    require((msg.value == uint256(0)));
    require((_a.svs.v139 == (uint256(keccak256(abi.encode(_a.msg.v210, _a.msg.v211))))));
    emit e6(_a);
    a3 memory la;
    la.svs.v48 = _a.svs.v48;
    la.svs.v47 = _a.svs.v47;
    la.svs.v82 = _a.svs.v82;
    la.msg.v90 = ((safeAdd((_a.svs.v182), msg.value)));
    la.msg.v91 = ((safeAdd(_a.msg.v211, (safeSub(uint256(4), _a.svs.v175)))) % uint256(3));
    l3(la);
    
     }
  
  
  struct a7 {
    a5postsvs svs;
     }
  event e7(a7 _a);
  
  function m7(a7 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a.svs))));
    
    
    require(uint256(block.number) >= safeAdd(_a.svs._last, uint256(10)) && true);
    require((_a.svs.v82 == msg.sender));
    require((msg.value == uint256(0)));
    _a.svs.v82.transfer(((safeAdd((_a.svs.v182), msg.value))));
    emit e7(_a);
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
  struct a8 {
    a4postsvs svs;
     }
  event e8(a8 _a);
  
  function m8(a8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a.svs))));
    
    
    require(uint256(block.number) >= safeAdd(_a.svs._last, uint256(10)) && true);
    require((_a.svs.v47 == msg.sender));
    require((msg.value == uint256(0)));
    _a.svs.v47.transfer(((safeAdd((_a.svs.v146), msg.value))));
    emit e8(_a);
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
  struct a9 {
    a3postsvs svs;
     }
  event e9(a9 _a);
  
  function m9(a9 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a.svs))));
    
    
    require(uint256(block.number) >= safeAdd(_a.svs._last, uint256(10)) && true);
    require((_a.svs.v82 == msg.sender));
    require((msg.value == uint256(0)));
    _a.svs.v82.transfer(((safeAdd((_a.svs.v90), msg.value))));
    emit e9(_a);
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
  struct a10 {
    a1postsvs svs;
     }
  event e10(a10 _a);
  
  function m10(a10 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a.svs))));
    
    
    require(uint256(block.number) >= safeAdd(_a.svs._last, uint256(10)) && true);
    require((_a.svs.v47 == msg.sender));
    require((msg.value == uint256(0)));
    _a.svs.v47.transfer(((safeAdd((_a.svs.v54), msg.value))));
    emit e10(_a);
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
   }
