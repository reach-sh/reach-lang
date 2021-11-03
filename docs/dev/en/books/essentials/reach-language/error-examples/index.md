---
menuItem: mi-docs
---
# Error Examples

``` nonum
Error: This Reach compiled ETH backend does not match the expectations of this Reach standard library: expected 5, but got 4; update your compiler and recompile!
    at checkVersion (/stdlib/dist/cjs/shared_impl.js:365:15)
    at ensureConnectorAvailable (/stdlib/dist/cjs/shared_impl.js:358:30)
    at Object.contract (/stdlib/dist/cjs/ETH_like.js:535:68)
    at file:///app/index.mjs:47:21
    at processTicksAndRejections (internal/process/task_queues.js:95:5)
npm ERR! code ELIFECYCLE
npm ERR! errno 1
npm ERR! @reach-sh/solution@ index: `node --experimental-modules --unhandled-rejections=strict index.mjs "seller"`
npm ERR! Exit status 1
npm ERR! 
npm ERR! Failed at the @reach-sh/solution@ index script.
npm ERR! This is probably not a problem with npm. There is likely additional logging output above.

npm ERR! A complete log of this run can be found in:
npm ERR!     /root/.npm/_logs/2021-11-01T05_21_08_969Z-debug.log
ERROR: 1
```