import fs from 'fs-extra';

const XXX = (name) => (...args) => {
  const m = ['XXX', name, ...args];
  console.log(m);
  return JSON.stringify(m);
};

export const seclink = XXX('seclink');
export const defn = XXX('defn');
export const workshopDeps = XXX('workshopDeps');
export const workshopInit = XXX('workshopInit');
export const workshopWIP = XXX('workshopWIP');
export const errver = XXX('errver');
export const ref = XXX('ref');
export const code = async ( rp, from = undefined, to = undefined ) => {
  if ( from || to ) { console.log(['XXX', 'code', { from, to }]); }
  const rpp = `${reachRoot}${rp}`;
  const lang = ''; // XXX
  const c = await fs.readFile(rpp, 'utf8');
  return "```" + lang + "\n" + c + "\n```";
};

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
