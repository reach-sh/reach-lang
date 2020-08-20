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
  
  event e3(uint256 _bal, uint256 v67);
  
  function m3(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v67) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _last, v7, v5, v6, v16))));
    
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e3(address(this).balance, v67);
    current_state = uint256(keccak256(abi.encode(uint256(3), uint256(block.number), v7, v5, v6, v16, v67))); }
  
  event e4(uint256 _bal, uint256 v112);
  
  function m4(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v67, uint256 v112) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _last, v7, v5, v6, v16, v67))));
    
    require(msg.sender == v16);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require(((uint256(0) <= v112) ? (v112 < uint256(3)) : false));
    emit e4(address(this).balance, v112);
    current_state = uint256(keccak256(abi.encode(uint256(4), uint256(block.number), v7, v5, v6, v16, v67, v112))); }
  
  event e5(uint256 _bal, uint256 v154, uint256 v155);
  struct _F5 {
    uint256 v191;
    T0 v261;
    bool v195;
    bool v199; }
  function m5(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v67, uint256 v112, uint256 v154, uint256 v155) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _last, v7, v5, v6, v16, v67, v112))));
    _F5 memory _f;
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require((v67 == (uint256(keccak256(abi.encode(v154, v155))))));
    require(((uint256(0) <= v155) ? (v155 < uint256(3)) : false));
    _f.v195 = (uint256(0) <= v155) ? (v155 < uint256(3)) : false;
    _f.v199 = (uint256(0) <= v112) ? (v112 < uint256(3)) : false;
    if ((_f.v195 ? _f.v199 : false)) {
      _f.v191 = ((v155 + (uint256(4) - v112)) % uint256(3));
       }
    else {
      if (_f.v195) {
        _f.v191 = uint256(2);
         }
      else {
        if (_f.v199) {
          _f.v191 = uint256(0);
           }
        else {
          _f.v191 = uint256(1);
           }
         }
       }
    if ((_f.v191 == uint256(2))) {
      _f.v261 = T0((uint256(2) * v5), uint256(0));
       }
    else {
      if ((_f.v191 == uint256(0))) {
        _f.v261 = T0(uint256(0), (uint256(2) * v5));
         }
      else {
        _f.v261 = T0(v5, v5);
         }
       }
    v7.transfer((v6 + (_f.v261.elem0)));
    v16.transfer((_f.v261.elem1));
    emit e5(address(this).balance, v154, v155);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e6(uint256 _bal);
  
  function m6(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v67, uint256 v112) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _last, v7, v5, v6, v16, v67, v112))));
    
    require(msg.sender == v16);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v16.transfer((address(this).balance));
    emit e6(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e7(uint256 _bal);
  
  function m7(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16, uint256 v67) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _last, v7, v5, v6, v16, v67))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    emit e7(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v16) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _last, v7, v5, v6, v16))));
    
    require(msg.sender == v16);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v16.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v7, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _last, v7, v5, v6))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }