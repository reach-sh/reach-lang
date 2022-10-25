import * as backend from './build/index.main.mjs';
import { loadStdlib, ask } from "@reach-sh/stdlib";
const stdlib = loadStdlib(process.env);

const startCtc = async (ctc, deployerName, startupInteractName, iface) => {
  const flag = "startup success throw flag"
  const fullIface = {...iface}
  fullIface[startupInteractName] = (...args) => {throw flag;};
  try {
    await ctc.p[deployerName](fullIface);
  } catch (e) {
    if ( e !== flag) {throw e;}
  }
}

const acc = await stdlib.newTestAccount(stdlib.parseCurrency(100));
const ctc = acc.contract(backend);
await startCtc(ctc, "Deployer", "deployed",
               {getTuple: () => [3, 27, acc.getAddress(), 6]});

stdlib.assert(await ctc.unsafeViews.vs1(27), "vs1-t")
stdlib.assert(! await ctc.unsafeViews.vs1(97), "vs1-f")
stdlib.assert(await ctc.unsafeViews.vs2(27), "vs2-t")
stdlib.assert(! await ctc.unsafeViews.vs2(97), "vs2-f")
stdlib.assert(await ctc.unsafeViews.vs3(27), "vs3-t")
stdlib.assert(! await ctc.unsafeViews.vs3(97), "vs3-f")
stdlib.assert(await ctc.unsafeViews.vd1(27), "vd1-t")
stdlib.assert(! await ctc.unsafeViews.vd1(97), "vd1-f")
stdlib.assert(await ctc.unsafeViews.vd2(27), "vd2-t")
stdlib.assert(! await ctc.unsafeViews.vd2(97), "vd2-f")
stdlib.assert(await ctc.unsafeViews.vd3(27), "vd3-t")
stdlib.assert(! await ctc.unsafeViews.vd3(97), "vd3-f")
