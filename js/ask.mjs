import * as readline from 'readline';

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

const ask_ = async (q) => {
  return new Promise((resolve) => {
    rl.question(q + '\n', (ans) => {
      resolve(ans);
    });
  });
};

// The validator arg should:
// * return anything but `undefined` on success.
// * throw an Error on failure
// The validator's output will be returned.
export const ask = async (question, validator) => {
  validator = validator || ((x) => x);
  let result = undefined;
  do {
    try {
      result = validator(await (ask_(question)));
    } catch (err) {
      console.log(err.message);
      // TODO: better re-prompt
      question = `valid answer pls? > `;
    }
  } while (result === undefined);
  return result;
};

export const done = () => {
  rl.close();
};

// The answer arg be 'y' (true) or 'n' (false)
export const yesno = (answer) => {
  if (answer === 'y') {
    return true;
  } else if (answer === 'n') {
    return false;
  } else {
    throw Error(`Only y/n are acceptable.`);
  }
};
