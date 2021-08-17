import Enquirer from 'enquirer';

// Hides sensitive input on command line
export const ask_secret = async (p) => {
  const prompt = await Enquirer.prompt([{
    type: 'password',
    message: p + '\n',
    name: 'password'
  }]);
  return prompt.password;
};

// Use custom ask prompt for consistent UI and because
// libraries that hide prompt do not place nicely with
// our use of `readline` in `stdlib/ask`
export const ask = async (question, validator) => {
  const ask_ = async (p) =>
    (await Enquirer.prompt([{
      type: 'input',
      message: p + '\n',
      name: 'ans',
    }])).ans;
  const validator_ = validator || ((x) => x);
  let result = undefined;
  do {
    try {
      result = validator_(await (ask_(question)));
    } catch (err) {
      console.log(err);
      question = `valid answer pls? > `;
    }
  } while (result === undefined);
  return result;
};

export const getTestNetAccount = async (stdlib) => {
  const isAlgo = stdlib.connector == 'ALGO';
  const prompt = isAlgo ? 'mnemonic' : 'key';
  const secret = await ask_secret(`What is your secret ${prompt}?\n`);
  return (await isAlgo
    ? stdlib.newAccountFromMnemonic(secret)
    : stdlib.newAccountFromSecret(secret));
}
