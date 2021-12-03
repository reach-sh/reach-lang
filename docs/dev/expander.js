import fs from 'fs-extra';

const XXX = (name) => (...args) => {
  const m = ['XXX', name, ...args];
  console.log(m);
  return JSON.stringify(m);
};

export const seclink = XXX('seclink');
export const defn = XXX('defn');

export const workshopDeps = (pre) => {
  if ( pre === undefined ) {
    return `:::note\nThis workshop is independent of all others.\n:::`;
  } else {
    return `:::note\nThis workshop assumes that you have recently completed @{seclink(\"${pre}\")}.\n:::`;
  }
};

export const workshopInit = XXX('workshopInit');
export const workshopWIP = XXX('workshopWIP');
export const errver = XXX('errver');
export const ref = XXX('ref');

export const directive_note = (node) => {
  const data = node.data;
  data.hName = "div";
  data.hProperties = { class: "note" };
}
export const directive_testQ = (node) => {
  const data = node.data;
  data.hName = "testQ";
  console.log(['XXX', 'testQ']);
}
export const directive_testA = (node) => {
  const data = node.data;
  data.hName = "testA";
  console.log(['XXX', 'testA']);
}
