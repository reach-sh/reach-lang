// Automatically generated with Reach 0.1.0
pragma experimental ABIEncoderV2;

pragma solidity ^0.5.16;

contract Stdlib { }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() public payable {
    
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
  
  function m3(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _last, v7, v5, v6, v15))));
    
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e3(address(this).balance);
    l4(v7, v5, v6, v15, uint256(0), uint256(1)); }
  
  struct _F4 {
    T0 v281; }
  function l4(address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v54, uint256 v55) internal {
    _F4 memory _f;
    
    if ((v55 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encode(uint256(4), uint256(block.number), v7, v5, v6, v15, v54))); }
    else {
      if ((v55 == uint256(2))) {
        _f.v281 = T0((uint256(2) * v5), uint256(0));
         }
      else {
        if ((v55 == uint256(0))) {
          _f.v281 = T0(uint256(0), (uint256(2) * v5));
           }
        else {
          _f.v281 = T0(v5, v5);
           }
         }
      v7.transfer((v6 + (_f.v281.elem0)));
      v15.transfer((_f.v281.elem1));
      
      current_state = 0x0;
      selfdestruct(msg.sender); } }
  
  event e5(uint256 _bal, uint256 v102);
  
  function m5(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v54, uint256 v102) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _last, v7, v5, v6, v15, v54))));
    
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e5(address(this).balance, v102);
    current_state = uint256(keccak256(abi.encode(uint256(5), uint256(block.number), v7, v5, v6, v15, v102, v54))); }
  
  event e6(uint256 _bal, uint256 v139);
  
  function m6(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v102, uint256 v54, uint256 v139) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _last, v7, v5, v6, v15, v102, v54))));
    
    require(msg.sender == v15);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require(((uint256(0) <= v139) ? (v139 < uint256(3)) : false));
    emit e6(address(this).balance, v139);
    current_state = uint256(keccak256(abi.encode(uint256(6), uint256(block.number), v7, v5, v6, v15, v102, v139, v54))); }
  
  event e7(uint256 _bal, uint256 v173, uint256 v174);
  struct _F7 {
    uint256 v202;
    bool v207;
    bool v212; }
  function m7(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v102, uint256 v139, uint256 v54, uint256 v173, uint256 v174) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(6), _last, v7, v5, v6, v15, v102, v139, v54))));
    _F7 memory _f;
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require((v102 == (uint256(keccak256(abi.encode(v173, v174))))));
    require(((uint256(0) <= v174) ? (v174 < uint256(3)) : false));
    _f.v207 = (uint256(0) <= v174) ? (v174 < uint256(3)) : false;
    _f.v212 = (uint256(0) <= v139) ? (v139 < uint256(3)) : false;
    if ((_f.v207 ? _f.v212 : false)) {
      _f.v202 = ((v174 + (uint256(4) - v139)) % uint256(3));
       }
    else {
      if (_f.v207) {
        _f.v202 = uint256(2);
         }
      else {
        if (_f.v212) {
          _f.v202 = uint256(0);
           }
        else {
          _f.v202 = uint256(1);
           }
         }
       }
    emit e7(address(this).balance, v173, v174);
    l4(v7, v5, v6, v15, (uint256(1) + v54), _f.v202); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v102, uint256 v139, uint256 v54) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(6), _last, v7, v5, v6, v15, v102, v139, v54))));
    
    require(msg.sender == v15);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v15.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v102, uint256 v54) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _last, v7, v5, v6, v15, v102, v54))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10(uint256 _bal);
  
  function m10(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v54) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _last, v7, v5, v6, v15, v54))));
    
    require(msg.sender == v15);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v15.transfer((address(this).balance));
    emit e10(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e11(uint256 _bal);
  
  function m11(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _last, v7, v5, v6, v15))));
    
    require(msg.sender == v15);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v15.transfer((address(this).balance));
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