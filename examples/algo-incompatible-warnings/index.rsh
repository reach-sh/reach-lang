'reach 0.1';
// 'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ctc: Fun([], Contract),
    addrs: Fun([], Tuple(
                    Address,Address,Address,Address,Address,Address,Address,Address,Address,Address,
                    Address,Address,Address,Address,Address,Address,Address,Address,Address,Address,
                    Address,Address,Address,Address,Address,Address,Address,Address,Address,Address,
                    Address,Address,Address,Address,Address,Address,Address,Address,Address,Address,
                    Address,Address,Address,Address,Address,Address,Address,Address,Address,Address,
                    Address,Address,Address,Address,Address,Address,Address,Address,Address,Address,
                    Address,Address,Address,Address)),
  });

  init();
  A.publish();

  commit();
  A.only(() => {
    const [a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,
           a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,a40,a41,a42,a43,a44,a45,
           a46,a47,a48,a49,a50,a51,a52,a53,a54,a55,a56,a57,a58,a59,a60,a61,a62,a63]
           = declassify(interact.addrs());
  });
  A.publish(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12, a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,
            a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,a40,a41,a42,a43,a44,
            a45,a46,a47,a48,a49,a50,a51,a52,a53,a54,a55,a56,a57,a58,a59,a60,a61,a62,a63);

  commit();
  A.only(() => {
    const [a0_,a1_,a2_,a3_,a4_,a5_,a6_,a7_,a8_,a9_,a10_,a11_,a12_,a13_,a14_,a15_,a16_,a17_,a18_,
           a19_,a20_,a21_,a22_,a23_,a24_,a25_,a26_,a27_,a28_,a29_,a30_,a31_,a32_,a33_,a34_,a35_,
           a36_,a37_,a38_,a39_,a40_,a41_,a42_,a43_,a44_,a45_,a46_,a47_,a48_,a49_,a50_,a51_,a52_,
           a53_,a54_,a55_,a56_,a57_,a58_,a59_,a60_,a61_,a62_,a63_] = declassify(interact.addrs());
  });
  A.publish(a0_,a1_,a2_,a3_,a4_,a5_,a6_,a7_,a8_,a9_,a10_,a11_,a12_, a13_,a14_,a15_,a16_,a17_,a18_,
            a19_,a20_,a21_,a22_,a23_,a24_,a25_,a26_,a27_,a28_,a29_,a30_,a31_,a32_,a33_,a34_,a35_,
            a36_,a37_,a38_,a39_,a40_,a41_,a42_,a43_,a44_,a45_,a46_,a47_,a48_,a49_,a50_,a51_,a52_,
            a53_,a54_,a55_,a56_,a57_,a58_,a59_,a60_,a61_,a62_,a63_);

  commit();
  A.only(() => {
    const ctc = declassify(interact.ctc());
  })
  A.publish(ctc);
  const rctc = remote(ctc, { f: Fun([], Null) });
  rctc.f();
  rctc.f();

  commit()
  exit();
});
