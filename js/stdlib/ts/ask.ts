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

/**
 * is an asynchronous function that asks a question on the
 * console.
 *
 * @param {string} question presented to user on console
 * @param {function=} validator should
 * - return anything but `undefined` on success.
 * - return an `Error` on failure.
 * @returns a `Promise` for the first result `validator`
 * done not error on.
 * @example
 * ```ts
 * const isAisha = await ask(
 *  'Are you Aisha?',
 *  validator
 * );
 * const person = isAisha ? 'Aisha' : 'Benjamin';
 * ```
 * @see https://docs.reach.sh/frontend/#p_153
 */
export const ask = async <T>(question: string, validator?: ((s: string) => T)): Promise<T> => {
  // Not sure how to require T=string if validator is undefined.
  // This is not type safe.
  // @ts-ignore
  const validator_: ((s: string) => T) = validator || ((x: string) => x);
  let result = undefined;
  do {
    try {
      result = validator_(await (ask_(question)));
    } catch (err:any) {
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
