import readline from 'readline';

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

const ask_ = async (q: string): Promise<string> => {
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
export const ask = async <T>(question: string, validator?: ((s: string) => T)): Promise<T> => {
  // Not sure how to require T=string if validator is undefined.
  // This is not type safe.
  // @ts-ignore
  const validator_: ((s: string) => T) = validator || ((x: string) => x);
  let result = undefined;
  do {
    try {
      result = validator_(await (ask_(question)));
    } catch (err) {
      console.log(err.message);
      // TODO: better re-prompt
      question = `valid answer pls? > `;
    }
  } while (result === undefined);
  return result;
};

export const done = (): void => {
  rl.close();
};

// The answer arg be 'y' (true) or 'n' (false)
export const yesno = (answer: string): boolean => {
  if (answer === 'y') {
    return true;
  } else if (answer === 'n') {
    return false;
  } else {
    throw Error(`Only y/n are acceptable.`);
  }
};
