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
  struct a1 {
    uint256 _last;
    uint256 v5;
    uint256 v6; }
  
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a._last))));
    
    
    require(true && true);
    require(((_a.v5 + _a.v6) == (msg.value)));
    emit e1(address(this).balance, _a.v5, _a.v6);
    current_state = uint256(keccak256(abi.encode(uint256(1), uint256(block.number), msg.sender, _a.v5, _a.v6))); }
  
  event e2(uint256 _bal);
  struct a2 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6; }
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v7, _a.v5, _a.v6))));
    
    
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((_a.v5 == (msg.value)));
    emit e2(address(this).balance);
    current_state = uint256(keccak256(abi.encode(uint256(2), uint256(block.number), _a.v7, _a.v5, _a.v6, msg.sender))); }
  
  event e3(uint256 _bal);
  struct a3 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16; }
  
  function m3(a3 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _a._last, _a.v7, _a.v5, _a.v6, _a.v16))));
    
    require(msg.sender == _a.v7);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e3(address(this).balance);
    l4(a4(_a.v7, _a.v5, _a.v6, _a.v16, uint256(0), uint256(1))); }
  
  struct a4 {
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16;
    uint256 v71;
    uint256 v72; }
  struct _F4 {
    T0 v319; }
  function l4(a4 memory _a) internal {
    _F4 memory _f;
    
    if ((_a.v72 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encode(uint256(4), uint256(block.number), _a.v7, _a.v5, _a.v6, _a.v16, _a.v71))); }
    else {
      if ((_a.v72 == uint256(2))) {
        _f.v319 = T0((uint256(2) * _a.v5), uint256(0));
         }
      else {
        if ((_a.v72 == uint256(0))) {
          _f.v319 = T0(uint256(0), (uint256(2) * _a.v5));
           }
        else {
          _f.v319 = T0(_a.v5, _a.v5);
           }
         }
      _a.v7.transfer((_a.v6 + (_f.v319.elem0)));
      _a.v16.transfer((_f.v319.elem1));
      
      current_state = 0x0;
      selfdestruct(msg.sender); } }
  
  event e5(uint256 _bal, uint256 v119);
  struct a5 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16;
    uint256 v71;
    uint256 v119; }
  
  function m5(a5 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v7, _a.v5, _a.v6, _a.v16, _a.v71))));
    
    require(msg.sender == _a.v7);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e5(address(this).balance, _a.v119);
    current_state = uint256(keccak256(abi.encode(uint256(5), uint256(block.number), _a.v7, _a.v5, _a.v6, _a.v16, _a.v119, _a.v71))); }
  
  event e6(uint256 _bal, uint256 v164);
  struct a6 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16;
    uint256 v119;
    uint256 v71;
    uint256 v164; }
  
  function m6(a6 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v7, _a.v5, _a.v6, _a.v16, _a.v119, _a.v71))));
    
    require(msg.sender == _a.v16);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((uint256(0) == (msg.value)));
    require(((uint256(0) <= _a.v164) ? (_a.v164 < uint256(3)) : false));
    emit e6(address(this).balance, _a.v164);
    current_state = uint256(keccak256(abi.encode(uint256(6), uint256(block.number), _a.v7, _a.v5, _a.v6, _a.v16, _a.v119, _a.v164, _a.v71))); }
  
  event e7(uint256 _bal, uint256 v206, uint256 v207);
  struct a7 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16;
    uint256 v119;
    uint256 v164;
    uint256 v71;
    uint256 v206;
    uint256 v207; }
  struct _F7 {
    uint256 v243;
    bool v247;
    bool v251; }
  function m7(a7 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(6), _a._last, _a.v7, _a.v5, _a.v6, _a.v16, _a.v119, _a.v164, _a.v71))));
    _F7 memory _f;
    require(msg.sender == _a.v7);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((uint256(0) == (msg.value)));
    require((_a.v119 == (uint256(keccak256(abi.encode(_a.v206, _a.v207))))));
    require(((uint256(0) <= _a.v207) ? (_a.v207 < uint256(3)) : false));
    _f.v247 = (uint256(0) <= _a.v207) ? (_a.v207 < uint256(3)) : false;
    _f.v251 = (uint256(0) <= _a.v164) ? (_a.v164 < uint256(3)) : false;
    if ((_f.v247 ? _f.v251 : false)) {
      _f.v243 = ((_a.v207 + (uint256(4) - _a.v164)) % uint256(3));
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
    emit e7(address(this).balance, _a.v206, _a.v207);
    l4(a4(_a.v7, _a.v5, _a.v6, _a.v16, (uint256(1) + _a.v71), _f.v243)); }
  
  event e8(uint256 _bal);
  struct a8 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16;
    uint256 v119;
    uint256 v164;
    uint256 v71; }
  
  function m8(a8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(6), _a._last, _a.v7, _a.v5, _a.v6, _a.v16, _a.v119, _a.v164, _a.v71))));
    
    require(msg.sender == _a.v16);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v16.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  struct a9 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16;
    uint256 v119;
    uint256 v71; }
  
  function m9(a9 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v7, _a.v5, _a.v6, _a.v16, _a.v119, _a.v71))));
    
    require(msg.sender == _a.v7);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v7.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10(uint256 _bal);
  struct a10 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16;
    uint256 v71; }
  
  function m10(a10 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v7, _a.v5, _a.v6, _a.v16, _a.v71))));
    
    require(msg.sender == _a.v16);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v16.transfer((address(this).balance));
    emit e10(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e11(uint256 _bal);
  struct a11 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6;
    address payable v16; }
  
  function m11(a11 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _a._last, _a.v7, _a.v5, _a.v6, _a.v16))));
    
    require(msg.sender == _a.v16);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v16.transfer((address(this).balance));
    emit e11(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e12(uint256 _bal);
  struct a12 {
    uint256 _last;
    address payable v7;
    uint256 v5;
    uint256 v6; }
  
  function m12(a12 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v7, _a.v5, _a.v6))));
    
    require(msg.sender == _a.v7);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v7.transfer((address(this).balance));
    emit e12(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }