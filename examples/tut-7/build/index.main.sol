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
    
    current_state = uint256(keccak256(abi.encode(uint256(0), uint256(block.number))));
    
     }
  
  
  
  
  
  
  
  
  
  event e1(uint256 v34);
  struct a1 {
    uint256 _last;
    uint256 v34;
     }
  struct _F1 {
    uint256 v41;
     }
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a._last))));
    _F1 memory _f;
    
    require(true && true);
    require((msg.value == _a.v34));
    _f.v41 = safeAdd((uint256(0)), msg.value);
    emit e1(_a.v34);
    current_state = uint256(keccak256(abi.encode(uint256(1), uint256(block.number), _f.v41, msg.sender, _a.v34)));
    
     }
  
  
  event e2();
  struct a2 {
    uint256 _last;
    uint256 v41;
    address payable v35;
    uint256 v34;
     }
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v41, _a.v35, _a.v34))));
    
    
    require(true && uint256(block.number) < safeAdd(_a._last, uint256(10)));
    require((msg.value == _a.v34));
    emit e2();
    l3(a3(_a.v35, _a.v34, msg.sender, ((safeAdd((_a.v41), msg.value))), uint256(1)));
    
     }
  
  
  struct a3 {
    address payable v35;
    uint256 v34;
    address payable v45;
    uint256 v75;
    uint256 v76;
     }
  
  function l3(a3 memory _a)  internal {
    
    if ((_a.v76 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encode(uint256(3), uint256(block.number), _a.v35, _a.v34, _a.v45, _a.v75)));
       }
    else {
      ((_a.v76 == uint256(2)) ? _a.v35 : _a.v45).transfer((safeMul(uint256(2), _a.v34)));
      
      current_state = 0x0;
      selfdestruct(msg.sender);
       }
     }
  
  
  event e4(uint256 v95);
  struct a4 {
    uint256 _last;
    address payable v35;
    uint256 v34;
    address payable v45;
    uint256 v75;
    uint256 v95;
     }
  struct _F4 {
    uint256 v123;
     }
  function m4(a4 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a._last, _a.v35, _a.v34, _a.v45, _a.v75))));
    _F4 memory _f;
    require(msg.sender == _a.v35);
    require(true && uint256(block.number) < safeAdd(_a._last, uint256(10)));
    require((msg.value == uint256(0)));
    _f.v123 = safeAdd((_a.v75), msg.value);
    emit e4(_a.v95);
    current_state = uint256(keccak256(abi.encode(uint256(4), uint256(block.number), _f.v123, _a.v35, _a.v34, _a.v45, _a.v95)));
    
     }
  
  
  event e5(uint256 v127);
  struct a5 {
    uint256 _last;
    uint256 v123;
    address payable v35;
    uint256 v34;
    address payable v45;
    uint256 v95;
    uint256 v127;
     }
  struct _F5 {
    uint256 v155;
     }
  function m5(a5 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v123, _a.v35, _a.v34, _a.v45, _a.v95))));
    _F5 memory _f;
    require(msg.sender == _a.v45);
    require(true && uint256(block.number) < safeAdd(_a._last, uint256(10)));
    require((msg.value == uint256(0)));
    _f.v155 = safeAdd((_a.v123), msg.value);
    emit e5(_a.v127);
    current_state = uint256(keccak256(abi.encode(uint256(5), uint256(block.number), _f.v155, _a.v35, _a.v34, _a.v45, _a.v95, _a.v127)));
    
     }
  
  
  event e6(uint256 v158, uint256 v159);
  struct a6 {
    uint256 _last;
    uint256 v155;
    address payable v35;
    uint256 v34;
    address payable v45;
    uint256 v95;
    uint256 v127;
    uint256 v158;
    uint256 v159;
     }
  
  function m6(a6 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v155, _a.v35, _a.v34, _a.v45, _a.v95, _a.v127))));
    
    require(msg.sender == _a.v35);
    require(true && uint256(block.number) < safeAdd(_a._last, uint256(10)));
    require((msg.value == uint256(0)));
    require((_a.v95 == (uint256(keccak256(abi.encode(_a.v158, _a.v159))))));
    emit e6(_a.v158, _a.v159);
    l3(a3(_a.v35, _a.v34, _a.v45, ((safeAdd((_a.v155), msg.value))), ((safeAdd(_a.v159, (safeSub(uint256(4), _a.v127)))) % uint256(3))));
    
     }
  
  
  event e7();
  struct a7 {
    uint256 _last;
    uint256 v155;
    address payable v35;
    uint256 v34;
    address payable v45;
    uint256 v95;
    uint256 v127;
     }
  
  function m7(a7 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v155, _a.v35, _a.v34, _a.v45, _a.v95, _a.v127))));
    
    require(msg.sender == _a.v45);
    require(uint256(block.number) >= safeAdd(_a._last, uint256(10)) && true);
    require((msg.value == uint256(0)));
    _a.v45.transfer(((safeAdd((_a.v155), msg.value))));
    emit e7();
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
  event e8();
  struct a8 {
    uint256 _last;
    uint256 v123;
    address payable v35;
    uint256 v34;
    address payable v45;
    uint256 v95;
     }
  
  function m8(a8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v123, _a.v35, _a.v34, _a.v45, _a.v95))));
    
    require(msg.sender == _a.v35);
    require(uint256(block.number) >= safeAdd(_a._last, uint256(10)) && true);
    require((msg.value == uint256(0)));
    _a.v35.transfer(((safeAdd((_a.v123), msg.value))));
    emit e8();
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
  event e9();
  struct a9 {
    uint256 _last;
    address payable v35;
    uint256 v34;
    address payable v45;
    uint256 v75;
     }
  
  function m9(a9 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a._last, _a.v35, _a.v34, _a.v45, _a.v75))));
    
    require(msg.sender == _a.v45);
    require(uint256(block.number) >= safeAdd(_a._last, uint256(10)) && true);
    require((msg.value == uint256(0)));
    _a.v45.transfer(((safeAdd((_a.v75), msg.value))));
    emit e9();
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
  event e10();
  struct a10 {
    uint256 _last;
    uint256 v41;
    address payable v35;
    uint256 v34;
     }
  
  function m10(a10 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v41, _a.v35, _a.v34))));
    
    require(msg.sender == _a.v35);
    require(uint256(block.number) >= safeAdd(_a._last, uint256(10)) && true);
    require((msg.value == uint256(0)));
    _a.v35.transfer(((safeAdd((_a.v41), msg.value))));
    emit e10();
    current_state = 0x0;
    selfdestruct(msg.sender);
    
     }
  
  
   }
