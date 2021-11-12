'reach 0.1';

const MAX = 20;
const LEN = 64;
const Word = Bytes(LEN);
const blank =
  LEN === 64 ?
  '                                                                ' :
  Word.pad('');

export const main = Reach.App(() => {
  const Oracle = Participant('Oracle', {
    ...hasConsoleLogger,
    i: UInt,
    getWord: Fun([Array(Word, MAX)], Word),
  });

  deploy();
  Oracle.interact.log('Deploying...');
  Oracle.only(() => {
    const i = declassify(interact.i);
    assume(i < MAX);
  })
  Oracle.publish(i);
  require(i < MAX);

  var [ dialog, idx ] = [ Array.replicate(MAX, blank), 0 ];
  invariant(balance() == 0);
  while ( idx < i ) {
    commit();
    Oracle.only(() => {
      const w = declassify(interact.getWord(dialog))
    });
    Oracle.publish(w);
    const dialogp = dialog.set(idx, w);
    Oracle.interact.log(dialogp);
    [ dialog, idx ] = [ dialogp, idx + 1 ];
    continue;
  }
  commit();
  exit();
});
