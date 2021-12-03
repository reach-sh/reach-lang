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
    return `:::note\nThis workshop is independent of all others.\n:::\n`;
  } else {
    return `:::note\nThis workshop assumes that you have recently completed @{seclink(\"${pre}\")}.\n:::\n`;
  }
};

export const workshopInit = XXX('workshopInit');

export const workshopWIP = (dir) => {
  const d = `examples/${dir}`;
  const more = dir ? `If you'd like to see a draft version of our code, please visit [\`${d}\`](http://github.com/reach-sh/reach-lang/tree/master/${d}).\n` : '';
  return `:::note\nThis page is a placeholder for a future more detailed workshop.\nYou could try to implement it yourself though, given the sketch above!\n${more}:::\n`;
};

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
