import * as ask from '@reach-sh/stdlib/ask.mjs';

export const getTestNetAccount = async (stdlib) => {
  const isAlgo = stdlib.connector == 'ALGO';
  const prompt = isAlgo ? 'mnemonic' : 'key';
  const secret = await ask.ask(`What is your secret ${prompt}?`);
  return (await isAlgo
    ? stdlib.newAccountFromMnemonic(secret)
    : stdlib.newAccountFromSecret(secret));
}
