'reach 0.1';
'use strict';

const TIMEOUT_PERIOD = 60 * 1000;
//const MINIMUM_FEE = 1000;

//-----------------------------------------------------------------------------
// Token operative & compliance parameters
//-----------------------------------------------------------------------------

const TokenParameters = Object({
  maxTransfers: UInt,
})

//-----------------------------------------------------------------------------
// Interaction interfaces
//-----------------------------------------------------------------------------

const IManager = {
  getBootstrapParams: Fun([], Tuple(Token, TokenParameters)),
  setupComplete: Fun([], Null)
}

const ISender = {
  getAxfer: Fun([], Tuple(UInt))
}

const IReceiver = {
  accAxfer: Fun([UInt], Bool)
}

//-----------------------------------------------------------------------------
// Helper functions
//-----------------------------------------------------------------------------
function validateParams(params) {
  assume(params.maxTransfers >= 1);
}

//-----------------------------------------------------------------------------
// DApp Entry point
//-----------------------------------------------------------------------------
export const main = Reach.App(
  {}, [
    Participant('Manager', IManager),
    ParticipantClass('Sender', ISender),
    ParticipantClass('Receiver', IReceiver)],
  (Manager, Sender, Receiver) => {

    // Step1:Manager must set token parameters

    Manager.only(() => {
      const [token, params] = declassify(interact.getBootstrapParams());
      validateParams(params);
      interact.setupComplete();
    })
    Manager.publish(token, params);

    var run = true
    invariant (balance() == 0 && balance(token) == 0)
    while(run) {
      Sender.only(() => {
        
      })

      Receiver.only(() => {

      })

      commit()
      Anybody.publish()
      
      continue;
    }

    commit();
  }
);

