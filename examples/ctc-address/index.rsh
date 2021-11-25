'reach 0.1';

export const main = Reach.App(() => {
  const common = {
    showCtcInfo: Fun([Contract], Null),
    showAddress: Fun([Address], Null),
    getCT: Fun([], Contract),
    getAddr: Fun([], Address),
    doTransfer: Fun([Address], Null),
  };
  const A = Participant('Alice', common);
  const B = Participant('Bob', common);
  deploy();

  A.publish();
  const info = getContract();
  const addr = getAddress();

  A.only(() => {
    interact.showCtcInfo(info);
    interact.showAddress(addr);
  });

  commit();

  B.only(() => {
    const ctcInfo = declassify(interact.getCT());
    assume(ctcInfo == info, "getContract() == ctc.getInfo()");
    const address = declassify(interact.getAddr());
    assume(addr == address, "getAddress() == ctc.getContractAddress()");
    interact.doTransfer(A);
  });
  B.publish(address, ctcInfo);

  require(info == ctcInfo, "getContract() == ctc.getInfo()");
  require(addr == address, "getAddress() == ctc.getContractAddress()");

  B.only(() => {
    interact.showCtcInfo(info);
    interact.showAddress(addr);
  });

  commit();

  exit();
});
