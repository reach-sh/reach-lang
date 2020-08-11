// Automatically generated with Reach 0.1.0

pragma solidity ^0.5.11;

contract Stdlib { }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() public payable {
    
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), uint256(block.number)))); }
  
  
  
  
  
  
  struct T0 {
    uint256 elem0;
    uint256 elem1; }
  
  event e1(uint256 _bal, uint256 v5, uint256 v6);
  
  function m1(uint256 _last, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    
    
    require(true && true);
    require(((v5 + v6) == (msg.value)));
    emit e1(address(this).balance, v5, v6);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), msg.sender, v5, v6))); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last, address payable v7, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v7, v5, v6))));
    
    
    require(true && uint256(block.number) < _last + uint256(10));
    require((v5 == (msg.value)));
    emit e2(address(this).balance);
    l3(v7, v5, v6, msg.sender, uint256(0), uint256(1)); }
  
  struct _F3 {
    T0 v226; }
  function l3(address payable v7, uint256 v5, uint256 v6, address payable v14, uint256 v25, uint256 v26) internal {
    _F3 memory _f;
    
    if ((v26 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v7, v5, v6, v14, v25))); }
    else {
      if ((v26 == uint256(2))) {
        _f.v226 = T0((uint256(2) * v5), uint256(0));
         }
      else {
        if ((v26 == uint256(0))) {
          _f.v226 = T0(uint256(0), (uint256(2) * v5));
           }
        else {
          _f.v226 = T0(v5, v5);
           }
         }
      v7.transfer((v6 + (_f.v226.elem0)));
      v14.transfer((_f.v226.elem1));
      
      current_state = 0x0;
      selfdestruct(msg.sender); } }
  
  event e4(uint256 _bal, uint256 v72);
  
  function m4(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v14, uint256 v25, uint256 v72) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v7, v5, v6, v14, v25))));
    
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e4(address(this).balance, v72);
    current_state = uint256(keccak256(abi.encodePacked(uint256(4), uint256(block.number), v7, v5, v6, v14, v72, v25))); }
  
  event e5(uint256 _bal, uint256 v101);
  
  function m5(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v14, uint256 v72, uint256 v25, uint256 v101) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v7, v5, v6, v14, v72, v25))));
    
    require(msg.sender == v14);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require(((uint256(0) <= v101) ? (v101 < uint256(3)) : false));
    emit e5(address(this).balance, v101);
    current_state = uint256(keccak256(abi.encodePacked(uint256(5), uint256(block.number), v7, v5, v6, v14, v72, v101, v25))); }
  
  event e6(uint256 _bal, uint256 v127, uint256 v128);
  struct _F6 {
    uint256 v148;
    bool v153;
    bool v158; }
  function m6(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v14, uint256 v72, uint256 v101, uint256 v25, uint256 v127, uint256 v128) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v7, v5, v6, v14, v72, v101, v25))));
    _F6 memory _f;
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require((v72 == (uint256(keccak256(abi.encodePacked(v127, v128))))));
    require(((uint256(0) <= v128) ? (v128 < uint256(3)) : false));
    _f.v153 = (uint256(0) <= v128) ? (v128 < uint256(3)) : false;
    _f.v158 = (uint256(0) <= v101) ? (v101 < uint256(3)) : false;
    if ((_f.v153 ? _f.v158 : false)) {
      _f.v148 = ((v128 + (uint256(4) - v101)) % uint256(3));
       }
    else {
      if (_f.v153) {
        _f.v148 = uint256(2);
         }
      else {
        if (_f.v158) {
          _f.v148 = uint256(0);
           }
        else {
          _f.v148 = uint256(1);
           }
         }
       }
    emit e6(address(this).balance, v127, v128);
    l3(v7, v5, v6, v14, (uint256(1) + v25), _f.v148); }
  
  event e7(uint256 _bal);
  
  function m7(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v14, uint256 v72, uint256 v101, uint256 v25) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v7, v5, v6, v14, v72, v101, v25))));
    
    require(msg.sender == v14);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v14.transfer((address(this).balance));
    emit e7(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v14, uint256 v72, uint256 v25) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v7, v5, v6, v14, v72, v25))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v14, uint256 v25) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v7, v5, v6, v14, v25))));
    
    require(msg.sender == v14);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v14.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10(uint256 _bal);
  
  function m10(uint256 _last, address payable v7, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v7, v5, v6))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    emit e10(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }