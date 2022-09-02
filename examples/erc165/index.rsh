// EIP-165 Specification
// https://eips.ethereum.org/EIPS/eip-165
'reach 0.1';

const forever = (f, initial = []) => {
  var lvs = initial;
  invariant(balance() == 0);
  while (true) {
    commit();
    const lvsP = f(...lvs);
    lvs = lvsP == null ? [] : lvsP;
    continue;
  }
  commit();
}

// The interface identifier for this interface is 0x01ffc9a7.
// You can calculate this by running bytes4(keccak256('supportsInterface(bytes4)'))
export const main = Reach.App(() => {
  const D = Participant('Deployer', { deployed: Fun(true, Null) });
  // This function must use at most 30,000 gas
  const A = API({ supportsInterface: Fun([Bytes(4)], Bool) });
  init();
  D.publish();
  D.interact.deployed();

  forever(() => {
    const [[interfaceId], k] = call(A.supportsInterface);
    k(Bytes.fromHex('0x01ffc9a7') === interfaceId &&
      Bytes.fromHex('0xffffffff') !== interfaceId);
  });
});
