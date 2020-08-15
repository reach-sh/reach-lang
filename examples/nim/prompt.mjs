import * as readline from 'readline';

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

export const yesno = (answer) => {
  if (answer === 'y') { return answer; }
  else if (answer === 'n') { return answer; }
  else { throw Error(`Only y/n are acceptable.`); }
};

const ask_ = async (q) => {
  return new Promise((resolve) => {
    rl.question(q, (ans) => {
      resolve(ans);
    });
  });
};

export const ask = async (question, validator) => {
  validator = validator || ((x) => x);
  let result = undefined;
  do {
    try {
      result = validator(await(ask_(question)));
    } catch (err) {
      console.log(err);
      // TODO: better re-prompt
      question = `valid answer pls? > `;
    }
  } while (result === undefined);
  return result;
};

export const noFurtherQuestions = () => {
  rl.close();
};
