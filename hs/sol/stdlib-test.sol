pragma solidity ^0.5.11;

import "stdlib.sol";

contract StdlibTest is Stdlib {
  event TestResult(bool x, bytes b);

  function TEST (bool x, bytes memory b) internal returns (bool, bytes memory) {
     emit TestResult(x, b);
     return (x, b); }

  function TEST_CAT_COMMITMENT () public returns (bool, bytes memory) {
    uint256 salt = 0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF;
    uint256 hand = 0x2;
    bytes memory message = abi.encodePacked(BCAT((abi.encodePacked(salt)), (abi.encodePacked(hand))));
    // bytes memory expected_msg = hex"00200123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0000000000000000000000000000000000000000000000000000000000000002";
    uint256 hash = uint256(keccak256(message));
    uint256 expected_hash = 0xc2ea5a84cdc7d7484ff41a5beb5e49a536aa59ef06218839f1f09fb7a5b3b10e;
    return TEST(hash == expected_hash, abi.encode(hash, message)); } }
