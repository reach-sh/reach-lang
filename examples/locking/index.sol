pragma abicoder v2;

pragma solidity ^0.8.0;

interface Server {
  function f() external returns (uint256);
  function x() external view returns (uint256);
}

contract WeirdContract {
  constructor() payable { }

  function checkFunReturn(bool succ, bytes memory returnData) internal pure returns (bytes memory) {
    if (succ) {
      return returnData;
    } else {
      if (returnData.length > 0) {
        assembly {
          let returnData_size := mload(returnData)
          revert(add(32, returnData), returnData_size)
        }
      } else {
        require(false);
      }
    }
  }

  function f(bool b) external returns (uint256) {
    address payable ctc = payable(msg.sender);
    if ( b ) {
      (bool ok, bytes memory ret) = ctc.call{value: uint256(0)}(abi.encodeWithSelector(Server.x.selector));
      checkFunReturn(ok, ret);
      return abi.decode(ret, (uint256));
    } else {
      (bool ok, bytes memory ret) = ctc.call{value: uint256(0)}(abi.encodeWithSelector(Server.f.selector));
      checkFunReturn(ok, ret);
      return abi.decode(ret, (uint256));
    }
  }
}
