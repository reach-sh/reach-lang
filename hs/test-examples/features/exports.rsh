'reach 0.1';

// See lib.rsh for a demo of export stuff

// export * from './lib.rsh';
// TODO: Unexpected token, MulToken at ./test-examples/features/exports.rsh:5:8

// export {x} from './lib.rsh';
// TODO:         error: ./test-examples/features/exports.rsh:8:1:export:
// Invalid Reach export syntax: JSExportFrom
// This one fails because the module loader needs to also notice this and load it

const y = 3;
export {y, y as z};

export const main = Reach.App(
  {}, [], () => {}
);
