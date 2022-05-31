# {nft-outline} NFT Auction API Tutorial Outline

1. NFT Auction as a basic DApp with three participants
    1. Serves as review of content learned in prior tutorials
    1. Review Participant Interact Interface, Interact Objects, Participants, `parseCurrency`, `formatCurrency`, and `newTestAccount`
    1. What is an NFT
        1. Introduce `getSale` method 
        1. Intuitive explanation of `launchToken`
    1. Create a working command-line test suite. (non-interactive)
1. Convert to API Bidders (attachers) with single Creator Participant (deployer)
    1. Process of evolving thinking from Participants to API
    1. Intuitive API explanation
        1. Rather than needing the contract to reach out to the frontend
        1. APIs allow frontends to ping the smart contract when needed
1. Introduce `parallelReduce`
    1. while and fork
    1. race of parallel participants
    1. invariants
1. Bidder attack vectors
1. `api` in `parallelReduce`
    1. architecture of an API
    1. Understand when the DApp is in local step or consensus step
    1. When to use `api_`
        1. Introduce `check` 
        1. A smart dynamic assertion
    1. timeout
1. Complete the API command-line version
1. Refactor frontend to be an interactive command-line test suite
1. Use `reach react` to create a GUI that runs on `Testnet`
1. Review and conclude