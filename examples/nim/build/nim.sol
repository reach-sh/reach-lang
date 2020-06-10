// Automatically generated with Reach 0.1.0

pragma solidity ^0.5.11;

pragma solidity ^0.5.11;

contract Stdlib {

  function INT_TO_BYTES (uint256 x)
    internal pure returns (bytes memory) {
    return abi.encodePacked(x); }

  function BCAT (bytes memory l, bytes memory r)
    internal pure returns (bytes memory) {
    return abi.encodePacked(uint16(l.length), l, r); }

  function BCAT_LEFT_LENGTH (bytes memory c)
    internal pure returns (uint16) {
      require(c.length >= 2);
      uint16 len = uint16(uint8(c[0]))*256+uint16(uint8(c[1])); // TODO: improve using some library on the net
      require(c.length >= 2 + uint(len));
      return len; }

  // TODO: optimize using tricks from the Internet? But not before there are tests somehow.
  function BYTES_SLICE (bytes memory _in, uint16 _start, uint16 _len)
    internal pure returns (bytes memory) {
      require(_in.length >= uint(_start) + uint(_len));
      bytes memory out = new bytes(_len);
      uint16 end = _start + _len;
      for (uint16 i = 0; i < end; i++) {
              out[i] = _in[i]; }
      return out; }

  function BCAT_LEFT (bytes memory c)
    internal pure returns (bytes memory) {
      return BYTES_SLICE(c, 2, BCAT_LEFT_LENGTH(c)); }

  function BCAT_RIGHT (bytes memory c)
    internal pure returns (bytes memory) {
      uint16 start = 2 + BCAT_LEFT_LENGTH(c);
      uint16 len = uint16(c.length) - start;
      return BYTES_SLICE(c, start, len); } }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() public payable {
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), uint256(block.number)))); }
  
  event e1(uint256 _bal, uint256 v3, uint256 v5, uint256 v8);
  struct _F1 {
    address payable v0; }
  function m1(uint256 _last, uint256 v3, uint256 v5, uint256 v8) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    _F1 memory _f;
    _f.v0 = msg.sender;
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == v3));
    emit e1(address(this).balance, v3, v5, v8);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), _f.v0, v3, v5, v8))); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    
    
    require(uint256(block.number) >= _last + uint256(10));
    emit e2(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e3(uint256 _bal, uint256 v13);
  struct _F3 {
    address payable v1; }
  function m3(uint256 _last, address payable v0, uint256 v3, uint256 v5, uint256 v8, uint256 v13) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v0, v3, v5, v8))));
    _F3 memory _f;
    _f.v1 = msg.sender;
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == v3));
    emit e3(address(this).balance, v13);
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v0, _f.v1, v3, v5, v8, v13))); }
  
  event e4(uint256 _bal);
  
  function m4(uint256 _last, address payable v0, uint256 v3, uint256 v5, uint256 v8) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v0, v3, v5, v8))));
    
    require(msg.sender == v0);
    require(uint256(block.number) >= _last + uint256(10));
    v0.transfer((address(this).balance));
    emit e4(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e5(uint256 _bal, uint256 v16);
  
  function m5(uint256 _last, address payable v0, address payable v1, uint256 v3, uint256 v5, uint256 v8, uint256 v13, uint256 v16) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v0, v1, v3, v5, v8, v13))));
    
    require(msg.sender == v0);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    require((v8 == (uint256(keccak256(abi.encodePacked(v16))))));
    emit e5(address(this).balance, v16);
    l7(v0, v1, v3, (((v16 + v13) % uint256(2)) == uint256(0)), v5, v5); }
  
  event e6(uint256 _bal);
  
  function m6(uint256 _last, address payable v0, address payable v1, uint256 v3, uint256 v5, uint256 v8, uint256 v13) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v0, v1, v3, v5, v8, v13))));
    
    require(msg.sender == v1);
    require(uint256(block.number) >= _last + uint256(10));
    v1.transfer((address(this).balance));
    emit e6(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  
  function l7(address payable v0, address payable v1, uint256 v3, bool v24, uint256 v25, uint256 v26) internal {
    
    if ((((v25 + v26) > uint256(0)) ? false : true)) {
      v0.transfer(((v24 ? uint256(2) : uint256(0)) * v3));
      v1.transfer(((v24 ? uint256(0) : uint256(2)) * v3));
      current_state = 0x0;
      selfdestruct(msg.sender); }
    else {
      if (v24) {
        current_state = uint256(keccak256(abi.encodePacked(uint256(5), uint256(block.number), v0, v1, v3, v24, v25, v26))); }
      else {
        current_state = uint256(keccak256(abi.encodePacked(uint256(5), uint256(block.number), v0, v1, v3, v24, v25, v26))); } } }
  
  event e8(uint256 _bal, bool v34, uint256 v36);
  
  function m8(uint256 _last, address payable v0, address payable v1, uint256 v3, bool v24, uint256 v25, uint256 v26, bool v34, uint256 v36) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v0, v1, v3, v24, v25, v26))));
    
    require(msg.sender == v0);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    emit e8(address(this).balance, v34, v36);
    l7(v0, v1, v3, (v34 ? (v24 ? false : true) : (v24 ? false : true)), (v34 ? (v25 - v36) : v25), (v34 ? v26 : (v26 - v36))); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v0, address payable v1, uint256 v3, bool v24, uint256 v25, uint256 v26) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v0, v1, v3, v24, v25, v26))));
    
    require(msg.sender == v1);
    require(uint256(block.number) >= _last + uint256(10));
    v1.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10(uint256 _bal, bool v50, uint256 v52);
  
  function m10(uint256 _last, address payable v0, address payable v1, uint256 v3, bool v24, uint256 v25, uint256 v26, bool v50, uint256 v52) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v0, v1, v3, v24, v25, v26))));
    
    require(msg.sender == v1);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    emit e10(address(this).balance, v50, v52);
    l7(v0, v1, v3, (v50 ? (v24 ? false : true) : (v24 ? false : true)), (v50 ? (v25 - v52) : v25), (v50 ? v26 : (v26 - v52))); }
  
  event e11(uint256 _bal);
  
  function m11(uint256 _last, address payable v0, address payable v1, uint256 v3, bool v24, uint256 v25, uint256 v26) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v0, v1, v3, v24, v25, v26))));
    
    require(msg.sender == v0);
    require(uint256(block.number) >= _last + uint256(10));
    v0.transfer((address(this).balance));
    emit e11(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }