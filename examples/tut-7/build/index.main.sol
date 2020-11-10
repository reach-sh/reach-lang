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
  
  constructor() payable {
    
    current_state = uint256(keccak256(abi.encode(uint256(0), uint256(block.number)))); }
  
  
  
  
  
  
  
  event e1(uint256 v33);
  struct a1 {
    uint256 _last;
    uint256 v33; }
  struct _F1 {
    uint256 v40; }
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a._last))));
    _F1 memory _f;
    
    require(true && true);
    require((msg.value == _a.v33));
    _f.v40 = safeAdd((uint256(0)), msg.value);
    emit e1(_a.v33);
    current_state = uint256(keccak256(abi.encode(uint256(1), uint256(block.number), _f.v40, msg.sender, _a.v33))); }
  
  event e2();
  struct a2 {
    uint256 _last;
    uint256 v40;
    address payable v34;
    uint256 v33; }
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v40, _a.v34, _a.v33))));
    
    
    require(true && uint256(block.number) < safeAdd(_a._last, uint256(10)));
    require((msg.value == _a.v33));
    emit e2();
    l3(a3(_a.v34, _a.v33, msg.sender, ((safeAdd((_a.v40), msg.value))), uint256(1))); }
  
  struct a3 {
    address payable v34;
    uint256 v33;
    address payable v43;
    uint256 v71;
    uint256 v72; }
  
  function l3(a3 memory _a)  internal {
    
    if ((_a.v72 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encode(uint256(3), uint256(block.number), _a.v34, _a.v33, _a.v43, _a.v71))); }
    else {
      ((_a.v72 == uint256(2)) ? _a.v34 : _a.v43).transfer((safeMul(uint256(2), _a.v33)));
      
      current_state = 0x0;
      selfdestruct(msg.sender); } }
  
  event e4(uint256 v90);
  struct a4 {
    uint256 _last;
    address payable v34;
    uint256 v33;
    address payable v43;
    uint256 v71;
    uint256 v90; }
  struct _F4 {
    uint256 v116; }
  function m4(a4 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a._last, _a.v34, _a.v33, _a.v43, _a.v71))));
    _F4 memory _f;
    require(msg.sender == _a.v34);
    require(true && uint256(block.number) < safeAdd(_a._last, uint256(10)));
    require((msg.value == uint256(0)));
    _f.v116 = safeAdd((_a.v71), msg.value);
    emit e4(_a.v90);
    current_state = uint256(keccak256(abi.encode(uint256(4), uint256(block.number), _f.v116, _a.v34, _a.v33, _a.v43, _a.v90))); }
  
  event e5(uint256 v119);
  struct a5 {
    uint256 _last;
    uint256 v116;
    address payable v34;
    uint256 v33;
    address payable v43;
    uint256 v90;
    uint256 v119; }
  struct _F5 {
    uint256 v145; }
  function m5(a5 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v116, _a.v34, _a.v33, _a.v43, _a.v90))));
    _F5 memory _f;
    require(msg.sender == _a.v43);
    require(true && uint256(block.number) < safeAdd(_a._last, uint256(10)));
    require((msg.value == uint256(0)));
    _f.v145 = safeAdd((_a.v116), msg.value);
    emit e5(_a.v119);
    current_state = uint256(keccak256(abi.encode(uint256(5), uint256(block.number), _f.v145, _a.v34, _a.v33, _a.v43, _a.v90, _a.v119))); }
  
  event e6(uint256 v147, uint256 v148);
  struct a6 {
    uint256 _last;
    uint256 v145;
    address payable v34;
    uint256 v33;
    address payable v43;
    uint256 v90;
    uint256 v119;
    uint256 v147;
    uint256 v148; }
  
  function m6(a6 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v145, _a.v34, _a.v33, _a.v43, _a.v90, _a.v119))));
    
    require(msg.sender == _a.v34);
    require(true && uint256(block.number) < safeAdd(_a._last, uint256(10)));
    require((msg.value == uint256(0)));
    require((_a.v90 == (uint256(keccak256(abi.encode(_a.v147, _a.v148))))));
    emit e6(_a.v147, _a.v148);
    l3(a3(_a.v34, _a.v33, _a.v43, ((safeAdd((_a.v145), msg.value))), ((safeAdd(_a.v148, (safeSub(uint256(4), _a.v119)))) % uint256(3)))); }
  
  event e7();
  struct a7 {
    uint256 _last;
    uint256 v145;
    address payable v34;
    uint256 v33;
    address payable v43;
    uint256 v90;
    uint256 v119; }
  
  function m7(a7 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v145, _a.v34, _a.v33, _a.v43, _a.v90, _a.v119))));
    
    require(msg.sender == _a.v43);
    require(uint256(block.number) >= safeAdd(_a._last, uint256(10)) && true);
    require((msg.value == uint256(0)));
    _a.v43.transfer(((safeAdd((_a.v145), msg.value))));
    emit e7();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e8();
  struct a8 {
    uint256 _last;
    uint256 v116;
    address payable v34;
    uint256 v33;
    address payable v43;
    uint256 v90; }
  
  function m8(a8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v116, _a.v34, _a.v33, _a.v43, _a.v90))));
    
    require(msg.sender == _a.v34);
    require(uint256(block.number) >= safeAdd(_a._last, uint256(10)) && true);
    require((msg.value == uint256(0)));
    _a.v34.transfer(((safeAdd((_a.v116), msg.value))));
    emit e8();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9();
  struct a9 {
    uint256 _last;
    address payable v34;
    uint256 v33;
    address payable v43;
    uint256 v71; }
  
  function m9(a9 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a._last, _a.v34, _a.v33, _a.v43, _a.v71))));
    
    require(msg.sender == _a.v43);
    require(uint256(block.number) >= safeAdd(_a._last, uint256(10)) && true);
    require((msg.value == uint256(0)));
    _a.v43.transfer(((safeAdd((_a.v71), msg.value))));
    emit e9();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10();
  struct a10 {
    uint256 _last;
    uint256 v40;
    address payable v34;
    uint256 v33; }
  
  function m10(a10 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v40, _a.v34, _a.v33))));
    
    require(msg.sender == _a.v34);
    require(uint256(block.number) >= safeAdd(_a._last, uint256(10)) && true);
    require((msg.value == uint256(0)));
    _a.v34.transfer(((safeAdd((_a.v40), msg.value))));
    emit e10();
    current_state = 0x0;
    selfdestruct(msg.sender); } }