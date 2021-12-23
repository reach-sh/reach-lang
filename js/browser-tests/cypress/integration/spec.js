/// <reference types="Cypress"" />

const rps = (choice, outcome) => {
  let reach;
  let backend;

  let setupAliceP;
  let addr;
  let mnem;

  let setupBobP;
  let accBob;

  const addrPrompt = 'paste the address';
  const mnemPrompt = 'paste the mnemonic';

  // requires setupAliceP
  const fakePrompt = (p) => {
    if (p.includes(addrPrompt)) { return addr; }
    else if (p.includes(mnemPrompt)) { return mnem; }
    else { throw Error(`unexpected prompt: ${p}`); }
  };

  // requires setupBobP
  const launchBob = (ctcInfo) => {
    const ctc = accBob.contract(backend, ctcInfo);
    const noop = () => null;
    ctc.p.Bob({
      ...reach.hasRandom,
      getHand: () => 1, // Bob always plays paper
      acceptWager: noop,
      informTimeout: noop,
      seeOutcome: noop,
    });
  };

  cy.visit('/', {
    onBeforeLoad(w) {
      cy.stub(w, 'prompt').callsFake(fakePrompt);
    },
    onLoad(w) {
      reach = w.reach;
      backend = w.backend;
      assert(reach && backend, 'reach && backend');

      // Don't give alice money because we want to see that Fund Account works
      setupAliceP = reach.createAccount().then(async (acc) => {
        addr = reach.formatAddress(acc);
        mnem = reach.unsafeGetMnemonic(acc);
        assert(addr && mnem, 'addr && mnem');
      });

      const amt = reach.parseCurrency(100);
      setupBobP = reach.newTestAccount(amt).then((acc) => {
        accBob = acc;
        assert(accBob, 'accBob');
      });
    }
  });

  // Apparently this is how you wait for promises
  // Don't use async/await or else bad things happen
  cy.then(() => setupAliceP).log('Alice acc is set up');
  cy.then(() => setupBobP).log('Bob acc is set up');

  cy.contains('button', 'Go').click();
  cy.contains('button', 'Fund Account').click();
  cy.contains('button', 'Deployer').click();
  cy.contains('button', 'Set wager').click();
  cy.contains('button', 'Deploy').click();

  cy.get('.ContractInfo').then(async (el) => {
    const ctcInfoStr = el.text();
    const ctcInfo = JSON.parse(ctcInfoStr);
    launchBob(ctcInfo);
  });
  cy.contains('button', choice).click();
  cy.contains(outcome);
}

describe('rps-9-web', () => {
  // Bob always plays Paper
  it('loses', () => rps('Rock', 'Bob wins'));
  it('wins', () => rps('Scissors', 'Alice wins'));
});
