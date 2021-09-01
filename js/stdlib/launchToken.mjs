// XXX remove this file
export default async function (stdlib, accCreator, name, sym) {
  return await stdlib.launchToken(accCreator, name, sym);
}
