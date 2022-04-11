import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import  { ask, yesno, done } from '@reach-sh/stdlib/ask.mjs';

const stdlib = loadStdlib(process.env);

const perksArtist = [
  ['Digital download', 'download', stdlib.parseCurrency(10)],
  ['Digital download + CD', 'downloadCd', stdlib.parseCurrency(20)],
  ['Digital download, CD, and personal thank you note on CD', 'cdThanks', stdlib.parseCurrency(50)],
  ['Digital download, CD, thank you note, and private performance', 'show', stdlib.parseCurrency(60)],
];

(async () => {
  console.log('Crowdfunding platform');
  const fmt = x => stdlib.formatCurrency(x, 4);
  const getBalance = async () => fmt(await stdlib.balanceOf(acc));

  const isArtist = await ask('Are you using the app as an artist?', yesno);
  const acc = await stdlib.newTestAccount(stdlib.parseCurrency(isArtist? 10 : 100));
  let ctc = null;

  if (isArtist) {
    ctc = acc.contract(backend);
    ctc.getInfo().then((info) => {
      console.log(`The contract is at ${JSON.stringify(stdlib.bigNumberToNumber(info))}`);
    })
  } else {
    const info  = await ask('Paste contract info', JSON.parse);
    ctc = acc.contract(backend, info);
  }

  const interact = { ...stdlib.hasRandom };

  if (isArtist) {
    interact.goal = await ask('How much is your goal?', stdlib.parseCurrency);
    interact.logAmount = async (amt) => {
      console.log('log amt:', fmt(amt));
    };
  } 
  
  if (isArtist){
    await ctc.p.Artist(interact);
  } else {
    const fan = ctc.a.Fan;
    const pickedIdx = await ask(`Pick your perks ${perksArtist.map((p, idx) => `${idx + 1} - ${p[0]}, $${fmt(p[2])}`)}`, x => {
      if(isNaN(Number(x))){
        throw Error(`${x} is not a number!`)
      }

      return x - 1;
    });

    await fan.pickPerk(perksArtist[pickedIdx]);
  }
  const balance = await getBalance(acc);
  console.log('Current Balance', balance)
  done();
})();
