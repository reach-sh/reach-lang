// vi: ft=javascript
'reach 0.1';

// import o from '@github.com:reach-sh/reach-lang#6c3dd0f/examples/exports/index.rsh';

const o = { b: { c: true }};

export const main = Reach.App({}, [], () => assert(o.b.c));
