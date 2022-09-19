import {loadStdlib} from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);

let mnemonic, secretKeyHex;

switch (stdlib.connector) {
case 'ETH':
case 'CFX':
  mnemonic = 'any sphere burst private pink little report cricket december actual screen toast two stage offer increase win sustain second aware spray shaft onion still';
  secretKeyHex = '0xddfe540fcdbe8dc55754a845b751534794f141e4b5de336ba94b9b775e06b709';
  break;
case 'ALGO':
  mnemonic = 'embark animal cancel category loyal mountain bamboo drastic embark animal cancel category loyal mountain bamboo drastic embark animal cancel category loyal mountain bamboo able luggage';
  secretKeyHex = '0x4242424242424242424242424242424242424242424242424242424242424242';
  break;
default:
  throw "unknown connector";
}

const secretKeyArray = Uint8Array.from(Buffer.from(secretKeyHex.slice(2), 'hex'));
const acc1 = await stdlib.newAccountFromMnemonic(mnemonic);
const acc2 = await stdlib.newAccountFromSecret(secretKeyHex);
const acc3 = await stdlib.newAccountFromSecret(secretKeyArray);

console.log(acc1.getAddress());
console.log(acc2.getAddress());
console.log(acc3.getAddress());

stdlib.assert(acc1.getAddress() === acc2.getAddress()
           && acc1.getAddress() === acc3.getAddress());
