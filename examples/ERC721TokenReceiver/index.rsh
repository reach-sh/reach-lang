"reach 0.1";

export const rch_ERC721_TokenReceiver = Reach.App(() => {

  const O = Participant("Owner", { sel: Bytes(4) });
  const E = Events({ GotAToken: [Address, Address, UInt256, BytesDyn] });
  const A = API({ onERC721Received: Fun([Address, Address, UInt256, BytesDyn], Bytes(4)) });
  init();

  // TODO: replace selector with Bytes(4).fromHex
  // const onERC721Received_selector = Bytes4.fromHex("0x150b7a02");
  // == bytes4(keccak256("onERC721Received(address,address,uint256,bytes)"))
  O.only(() => { const selector = declassify(interact.sel); });
  O.publish(selector);
  commit();

  const [[operator, from_, tokenId, data], k] = call(A.onERC721Received);
  E.GotAToken(operator, from_, tokenId, data);
  k(selector);
  commit();
});
