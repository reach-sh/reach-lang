---
author: Ivan Lin
hasOtp: false
menuItem: mi-articles
publishedDate: 2021-11-04T14:00:00
---

# How to sponsor a Reach contract on Conflux

This article shows you how to pay the gas and storage fees on behalf of participants that visit a Reach smart contract deployed on the Conflux MainNet or TestNet. You do this by adding the Reach contract information to the Conflux internal *SponsorControl* contract. For more information about Conflux internal contracts, see the [Internal Contract](https://developer.confluxnetwork.org/conflux-rust/internal_contract/internal_contract/) page on the [Conflux Developer Portal](https://developer.confluxnetwork.org/).

1. Open the JavaScript file (e.g. *index.js* or *index.mjs*) associated with your webapp.

1. Add the follow lines after your existing imports:

    ``` js
    const {Conflux} = require('js-conflux-sdk');
    const cfx = new Conflux({
      url: 'https://test.confluxrpc.com',
      logger: console,
      networkId: 1,
    });
    cfx.provider = window.conflux;
    ```

    * Line 1: Import the [Conflux JavaScript Library](https://github.com/Conflux-Chain/js-conflux-sdk).
    * Line 2: Instantiate a Conflux object.
    * Line 7: Set `provider` to the [ConfluxPortal](https://portal.confluxnetwork.org/) user's address.

1. Add the following lines after a participant deploys the Reach contract (within an `async` scope):

    ``` js
    const sponsor_contract = cfx.InternalContract('SponsorWhitelistControl');
    const ctcInfo = await ctc.getContractAddress();
    await sponsor_contract.addPrivilegeByAdmin(ctcInfo, ["0x0000000000000000000000000000000000000000"])
      .sendTransaction({from: reach.formatAddress(this.props.acc),
    });
    ```

    * Line 1: Get a reference to the Conflux internal *SponsorWhitelistControl* contract.
    * Line 2: Get the contract information of your Reach contract.
    * Line 3: Sponsor all addresses in the array (or, in this case, all addresses).
    * Line 4: Send a transaction signed by ConfluxPortal. `addPrivilegeByAdmin` returns the address of the initiator.

Below is an example from [Tutorial 9](https://github.com/reach-sh/reach-lang/tree/master/examples/rps-9-web):

``` js nonum
async deploy() {
    const ctc = this.props.acc.deploy(backend);
    this.setState({view: 'Deploying', ctc});
    this.wager = reach.parseCurrency(this.state.wager); 
    this.deadline = {ETH: 10, ALGO: 100, CFX: 1000}[reach.connector]; 
    backend.Alice(ctc, this);
    const sponsor_contract = cfx.InternalContract('SponsorWhitelistControl');
    const ctcInfo = await ctc.getContractAddress();
    await sponsor_contract.addPrivilegeByAdmin(ctcInfo, ["0x0000000000000000000000000000000000000000"]).sendTransaction({
      from: reach.formatAddress(this.props.acc),
    });
    const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
    this.setState({view: 'WaitingForAttacher', ctcInfoStr});
  }
```

