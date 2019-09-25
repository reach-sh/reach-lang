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
  
  event e1(uint256 v2, uint256 v3);
  function m1(uint256 _last, uint256 v2, uint256 v3) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    address payable v6 = msg.sender;
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == (v2 + v3)));
    emit e1(v2, v3);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), v2, v3, v6))); }
  
  event e2();
  function m2(uint256 _last) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    address payable v7 = msg.sender;
    require(uint256(block.number) >= _last + uint256(10));
    emit e2();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e3();
  function m3(uint256 _last, uint256 v2, uint256 v3, address payable v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v2, v3, v6))));
    address payable v12 = msg.sender;
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == v2));
    emit e3();
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v2, v3, v6, v12))); }
  
  event e4();
  function m4(uint256 _last, uint256 v2, uint256 v3, address payable v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v2, v3, v6))));
    require(msg.sender == v6);
    require(uint256(block.number) >= _last + uint256(10));
    v6.transfer((address(this).balance));
    emit e4();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e5(uint256 v31);
  function m5(uint256 _last, uint256 v2, uint256 v3, address payable v6, address payable v12, uint256 v31) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v2, v3, v6, v12))));
    require(msg.sender == v6);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    emit e5(v31);
    current_state = uint256(keccak256(abi.encodePacked(uint256(5), uint256(block.number), v2, v3, v6, v12, v31))); }
  
  event e6();
  function m6(uint256 _last, uint256 v2, uint256 v3, address payable v6, address payable v12) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v2, v3, v6, v12))));
    require(msg.sender == v12);
    require(uint256(block.number) >= _last + uint256(10));
    v12.transfer((address(this).balance));
    emit e6();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e7(uint256 v46);
  function m7(uint256 _last, uint256 v2, uint256 v3, address payable v6, address payable v12, uint256 v31, uint256 v46) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v2, v3, v6, v12, v31))));
    require(msg.sender == v12);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    require(((uint256(0) <= v46) ? (v46 < uint256(3)) : false));
    emit e7(v46);
    current_state = uint256(keccak256(abi.encodePacked(uint256(7), uint256(block.number), v2, v3, v6, v12, v31, v46))); }
  
  event e8();
  function m8(uint256 _last, uint256 v2, uint256 v3, address payable v6, address payable v12, uint256 v31) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v2, v3, v6, v12, v31))));
    require(msg.sender == v6);
    require(uint256(block.number) >= _last + uint256(10));
    v6.transfer((address(this).balance));
    emit e8();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 v53, uint256 v54);
  function m9(uint256 _last, uint256 v2, uint256 v3, address payable v6, address payable v12, uint256 v31, uint256 v46, uint256 v53, uint256 v54) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(7), _last, v2, v3, v6, v12, v31, v46))));
    require(msg.sender == v6);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    require((v31 == (uint256(keccak256(abi.encodePacked((BCAT((abi.encodePacked(v53)), (abi.encodePacked(v54))))))))));
    require(((uint256(0) <= v54) ? (v54 < uint256(3)) : false));
    v6.transfer((v3 + ((((((uint256(0) <= v54) ? (v54 < uint256(3)) : false) ? ((uint256(0) <= v46) ? (v46 < uint256(3)) : false) : false) ? ((v54 + (uint256(4) - v46)) % uint256(3)) : (((uint256(0) <= v54) ? (v54 < uint256(3)) : false) ? uint256(2) : (((uint256(0) <= v46) ? (v46 < uint256(3)) : false) ? uint256(0) : uint256(1)))) == uint256(2)) ? (uint256(2) * v2) : ((((((uint256(0) <= v54) ? (v54 < uint256(3)) : false) ? ((uint256(0) <= v46) ? (v46 < uint256(3)) : false) : false) ? ((v54 + (uint256(4) - v46)) % uint256(3)) : (((uint256(0) <= v54) ? (v54 < uint256(3)) : false) ? uint256(2) : (((uint256(0) <= v46) ? (v46 < uint256(3)) : false) ? uint256(0) : uint256(1)))) == uint256(0)) ? uint256(0) : v2))));
    v12.transfer(((((((uint256(0) <= v54) ? (v54 < uint256(3)) : false) ? ((uint256(0) <= v46) ? (v46 < uint256(3)) : false) : false) ? ((v54 + (uint256(4) - v46)) % uint256(3)) : (((uint256(0) <= v54) ? (v54 < uint256(3)) : false) ? uint256(2) : (((uint256(0) <= v46) ? (v46 < uint256(3)) : false) ? uint256(0) : uint256(1)))) == uint256(2)) ? uint256(0) : ((((((uint256(0) <= v54) ? (v54 < uint256(3)) : false) ? ((uint256(0) <= v46) ? (v46 < uint256(3)) : false) : false) ? ((v54 + (uint256(4) - v46)) % uint256(3)) : (((uint256(0) <= v54) ? (v54 < uint256(3)) : false) ? uint256(2) : (((uint256(0) <= v46) ? (v46 < uint256(3)) : false) ? uint256(0) : uint256(1)))) == uint256(0)) ? (uint256(2) * v2) : v2)));
    emit e9(v53, v54);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10();
  function m10(uint256 _last, uint256 v2, uint256 v3, address payable v6, address payable v12, uint256 v31, uint256 v46) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(7), _last, v2, v3, v6, v12, v31, v46))));
    require(msg.sender == v12);
    require(uint256(block.number) >= _last + uint256(10));
    v12.transfer((address(this).balance));
    emit e10();
    current_state = 0x0;
    selfdestruct(msg.sender); } }