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
  
  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))); }
  
  event e0(uint256 v2, uint256 v3);
  function m0(address payable pA, address payable pB, uint256 v2, uint256 v3) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))));
    require(msg.sender == pA);
    require(((msg.value) == (v2 + v3)));
    emit e0(v2, v3);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v2, v3))); }
  
  event e1();
  function m1(address payable pA, address payable pB, uint256 v2, uint256 v3) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v2, v3))));
    require(msg.sender == pB);
    require(((msg.value) == v2));
    emit e1();
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v2, v3))); }
  
  event e2(uint256 v28);
  function m2(address payable pA, address payable pB, uint256 v2, uint256 v3, uint256 v28) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v2, v3))));
    require(msg.sender == pA);
    require(((msg.value) == uint256(0)));
    emit e2(v28);
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), pA, pB, v2, v3, v28))); }
  
  event e3(uint256 v43);
  function m3(address payable pA, address payable pB, uint256 v2, uint256 v3, uint256 v28, uint256 v43) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), pA, pB, v2, v3, v28))));
    require(msg.sender == pB);
    require(((msg.value) == uint256(0)));
    require(((uint256(0) <= v43) ? (v43 < uint256(3)) : false));
    emit e3(v43);
    current_state = uint256(keccak256(abi.encodePacked(uint256(4), pA, pB, v2, v3, v28, v43))); }
  
  event e4(uint256 v50, uint256 v51);
  function m4(address payable pA, address payable pB, uint256 v2, uint256 v3, uint256 v28, uint256 v43, uint256 v50, uint256 v51) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), pA, pB, v2, v3, v28, v43))));
    require(msg.sender == pA);
    require(((msg.value) == uint256(0)));
    require((v28 == (uint256(keccak256(abi.encodePacked((BCAT((abi.encodePacked(v50)), (abi.encodePacked(v51))))))))));
    require(((uint256(0) <= v51) ? (v51 < uint256(3)) : false));
    bool v65 = (uint256(0) <= v51) ? (v51 < uint256(3)) : false;
    bool v68 = (uint256(0) <= v43) ? (v43 < uint256(3)) : false;
    uint256 v75 = (v65 ? v68 : false) ? ((v51 + (uint256(4) - v43)) % uint256(3)) : (v65 ? uint256(2) : (v68 ? uint256(0) : uint256(1)));
    bool v91 = v75 == uint256(2);
    bool v98 = v75 == uint256(0);
    bool v105 = v75 == uint256(2);
    bool v107 = v75 == uint256(0);
    pA.transfer((v3 + (v105 ? (uint256(2) * v2) : (v107 ? uint256(0) : v2))));
    pB.transfer((v105 ? uint256(0) : (v107 ? (uint256(2) * v2) : v2)));
    emit e4(v50, v51);
    current_state = 0x0;
    selfdestruct(address(pA)); } }