// SPDX-License-Identifier: Apache-2.0
pragma solidity ^0.8.26;

// Deploys the Reach-emitted contract (copied into src/ by run.sh).
// No forge-std dependency: plain requires keep the scratch project offline.
import {ReachContract, T0} from "../src/index.main.sol";

contract DeployTest {
    function test_deploy() public {
        // First publish: (time-check = 0 means "now", pay amt with the call).
        uint256 amt = 1 ether;
        ReachContract c = new ReachContract{value: amt}(T0(0, amt));
        require(address(c).code.length > 0, "no code at deployed address");
        require(address(c).balance == amt, "deposit not held by contract");
    }
}
