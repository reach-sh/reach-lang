// Automatically generated with Reach 0.1.0
pragma experimental ABIEncoderV2;

pragma solidity ^0.7.0;

contract Stdlib { }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() payable {
    
    current_state = uint256(keccak256(abi.encode(uint256(0), uint256(block.number)))); }
  
  
  
  
  
  
  struct T0 {
    uint256 elem0;
    uint256 elem1; }
  
  event e1(uint256 _bal, uint256 v5, uint256 v6);
  
  function m1(uint256 _last, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _last))));
    
    
    require(true && true);
    require(((v5 + v6) == (msg.value)));
    emit e1(address(this).balance, v5, v6);
    current_state = uint256(keccak256(abi.encode(uint256(1), uint256(block.number), msg.sender, v5, v6))); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last, address payable v7, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _last, v7, v5, v6))));
    
    
    require(true && uint256(block.number) < _last + uint256(10));
    require((v5 == (msg.value)));
    emit e2(address(this).balance);
    current_state = uint256(keccak256(abi.encode(uint256(2), uint256(block.number), v7, v5, v6, msg.sender))); }
  
  event e3(uint256 _bal);
  
  function m3(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _last, v7, v5, v6, v16))));
    
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e3(address(this).balance);
    l4(v7, v5, v6, v16, uint256(0), uint256(1)); }
  
  struct _F4 {
    T0 v319; }
  function l4(address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v71, uint256 v72) internal {
    _F4 memory _f;
    
    if ((v72 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encode(uint256(4), uint256(block.number), v7, v5, v6, v16, v71))); }
    else {
      if ((v72 == uint256(2))) {
        _f.v319 = T0((uint256(2) * v5), uint256(0));
         }
      else {
        if ((v72 == uint256(0))) {
          _f.v319 = T0(uint256(0), (uint256(2) * v5));
           }
        else {
          _f.v319 = T0(v5, v5);
           }
         }
      v7.transfer((v6 + (_f.v319.elem0)));
      v16.transfer((_f.v319.elem1));
      
      current_state = 0x0;
      selfdestruct(msg.sender); } }
  
  event e5(uint256 _bal, uint256 v119);
  
  function m5(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v71, uint256 v119) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _last, v7, v5, v6, v16, v71))));
    
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e5(address(this).balance, v119);
    current_state = uint256(keccak256(abi.encode(uint256(5), uint256(block.number), v7, v5, v6, v16, v119, v71))); }
  
  event e6(uint256 _bal, uint256 v164);
  
  function m6(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v119, uint256 v71, uint256 v164) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _last, v7, v5, v6, v16, v119, v71))));
    
    require(msg.sender == v16);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require(((uint256(0) <= v164) ? (v164 < uint256(3)) : false));
    emit e6(address(this).balance, v164);
    current_state = uint256(keccak256(abi.encode(uint256(6), uint256(block.number), v7, v5, v6, v16, v119, v164, v71))); }
  
  event e7(uint256 _bal, uint256 v206, uint256 v207);
  struct _F7 {
    uint256 v243;
    bool v247;
    bool v251; }
  function m7(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v119, uint256 v164, uint256 v71, uint256 v206, uint256 v207) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(6), _last, v7, v5, v6, v16, v119, v164, v71))));
    _F7 memory _f;
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require((v119 == (uint256(keccak256(abi.encode(v206, v207))))));
    require(((uint256(0) <= v207) ? (v207 < uint256(3)) : false));
    _f.v247 = (uint256(0) <= v207) ? (v207 < uint256(3)) : false;
    _f.v251 = (uint256(0) <= v164) ? (v164 < uint256(3)) : false;
    if ((_f.v247 ? _f.v251 : false)) {
      _f.v243 = ((v207 + (uint256(4) - v164)) % uint256(3));
       }
    else {
      if (_f.v247) {
        _f.v243 = uint256(2);
         }
      else {
        if (_f.v251) {
          _f.v243 = uint256(0);
           }
        else {
          _f.v243 = uint256(1);
           }
         }
       }
    emit e7(address(this).balance, v206, v207);
    l4(v7, v5, v6, v16, (uint256(1) + v71), _f.v243); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v119, uint256 v164, uint256 v71) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(6), _last, v7, v5, v6, v16, v119, v164, v71))));
    
    require(msg.sender == v16);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v16.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v119, uint256 v71) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _last, v7, v5, v6, v16, v119, v71))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10(uint256 _bal);
  
  function m10(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v71) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _last, v7, v5, v6, v16, v71))));
    
    require(msg.sender == v16);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v16.transfer((address(this).balance));
    emit e10(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e11(uint256 _bal);
  
  function m11(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _last, v7, v5, v6, v16))));
    
    require(msg.sender == v16);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v16.transfer((address(this).balance));
    emit e11(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e12(uint256 _bal);
  
  function m12(uint256 _last, address payable v7, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _last, v7, v5, v6))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    emit e12(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }