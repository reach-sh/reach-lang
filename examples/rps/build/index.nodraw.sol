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
  
  event e1(uint256 _bal, uint256 v4, uint256 v5);
  
  function m1(uint256 _last, uint256 v4, uint256 v5) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    
    
    require(true && true);
    require(((v4 + v5) == (msg.value)));
    emit e1(address(this).balance, v4, v5);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), msg.sender, v4, v5))); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last, address payable v6, uint256 v4, uint256 v5) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v6, v4, v5))));
    
    
    require(true && uint256(block.number) < _last + uint256(10));
    require((v4 == (msg.value)));
    emit e2(address(this).balance);
    l3(v6, v4, v5, msg.sender, uint256(0), uint256(1)); }
  
  struct _F3 {
    T0 v257; }
  function l3(address payable v6, uint256 v4, uint256 v5, address payable v13, uint256 v32, uint256 v33) internal {
    _F3 memory _f;
    
    if ((v33 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v6, v4, v5, v13, v32))); }
    else {
      if ((v33 == uint256(2))) {
        _f.v257 = T0((uint256(2) * v4), uint256(0));
         }
      else {
        if ((v33 == uint256(0))) {
          _f.v257 = T0(uint256(0), (uint256(2) * v4));
           }
        else {
          _f.v257 = T0(v4, v4);
           }
         }
      v6.transfer((v5 + (_f.v257.elem0)));
      v13.transfer((_f.v257.elem1));
      
      current_state = 0x0;
      selfdestruct(msg.sender); } }
  
  event e4(uint256 _bal, uint256 v79);
  
  function m4(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v13, uint256 v32, uint256 v79) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v6, v4, v5, v13, v32))));
    
    require(msg.sender == v6);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e4(address(this).balance, v79);
    current_state = uint256(keccak256(abi.encodePacked(uint256(4), uint256(block.number), v6, v4, v5, v13, v79, v32))); }
  
  event e5(uint256 _bal, uint256 v116);
  
  function m5(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v13, uint256 v79, uint256 v32, uint256 v116) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v6, v4, v5, v13, v79, v32))));
    
    require(msg.sender == v13);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require(((uint256(0) <= v116) ? (v116 < uint256(3)) : false));
    emit e5(address(this).balance, v116);
    current_state = uint256(keccak256(abi.encodePacked(uint256(5), uint256(block.number), v6, v4, v5, v13, v79, v116, v32))); }
  
  event e6(uint256 _bal, uint256 v150, uint256 v151);
  struct _F6 {
    uint256 v179;
    bool v184;
    bool v189; }
  function m6(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v13, uint256 v79, uint256 v116, uint256 v32, uint256 v150, uint256 v151) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v6, v4, v5, v13, v79, v116, v32))));
    _F6 memory _f;
    require(msg.sender == v6);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require((v79 == (uint256(keccak256(abi.encodePacked(v150, v151))))));
    require(((uint256(0) <= v151) ? (v151 < uint256(3)) : false));
    _f.v184 = (uint256(0) <= v151) ? (v151 < uint256(3)) : false;
    _f.v189 = (uint256(0) <= v116) ? (v116 < uint256(3)) : false;
    if ((_f.v184 ? _f.v189 : false)) {
      _f.v179 = ((v151 + (uint256(4) - v116)) % uint256(3));
       }
    else {
      if (_f.v184) {
        _f.v179 = uint256(2);
         }
      else {
        if (_f.v189) {
          _f.v179 = uint256(0);
           }
        else {
          _f.v179 = uint256(1);
           }
         }
       }
    emit e6(address(this).balance, v150, v151);
    l3(v6, v4, v5, v13, (uint256(1) + v32), _f.v179); }
  
  event e7(uint256 _bal);
  
  function m7(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v13, uint256 v79, uint256 v116, uint256 v32) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v6, v4, v5, v13, v79, v116, v32))));
    
    require(msg.sender == v13);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v13.transfer((address(this).balance));
    emit e7(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v13, uint256 v79, uint256 v32) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v6, v4, v5, v13, v79, v32))));
    
    require(msg.sender == v6);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v6.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v13, uint256 v32) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v6, v4, v5, v13, v32))));
    
    require(msg.sender == v13);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v13.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10(uint256 _bal);
  
  function m10(uint256 _last, address payable v6, uint256 v4, uint256 v5) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v6, v4, v5))));
    
    require(msg.sender == v6);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v6.transfer((address(this).balance));
    emit e10(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }