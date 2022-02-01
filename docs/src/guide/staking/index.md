# {#guide-staking} Staking and Unstaking Tokens

Reach makes DApp development safer, faster, and more secure.
This guide offers a real example of the advantages of using Reach.

Here's an example provided by Austin Wilshire at [xBacked](https://twitter.com/xbacked). 
Follow Austin on Twitter [@awoldes](https://twitter.com/awoldes).

Imagine you want to stake a token and at a later date unstake it with all the rewards.
Let's look at a snippet from the perspective of a Deployer. 

As the deployer, you want to set the reward rate, `{!rsh} Token` ID of the token being staked, the `{!rsh} Token` ID of the rewarded token, and the initial supply of the reward token.

``` rsh
// Initiate the Deployer's local state
Deployer.only(() => {
    // this = address doing transaction
    const deployerAddr = this;
    // Declassify data and store in constants
    const stakeToken = declassify(interact.setStakeToken());
    const rewardRate = declassify(interact.setRewardRate());
    const rewardToken = declassify(interact.setRewardToken());
    const initialSupply = declassify(interact.setInitialSupply());
});

// Publish this information to global state
Deployer.publish(
    deployerAddr,
    stakeToken,
    rewardRate,
    rewardToken,
    initialSupply,
);
``` 

The configurable contract is deployed. The next step is to enable users to interact with it. 
In Reach, the solution is to add an API call so that when the user calls the function, they will pay `{!rsh} tokenAmt` of the `{!rsh} stakeToken` in the Deployer set. 

``` rsh
.api(User.stakeTokens,
  (tokenAmt) => {
      // validation that a user must stake more than 1 token
      // they will receive the message on the frontend if it fails
      assume(tokenAmt >= 1, 'Must stake at least 1 token');
  },
  (tokenAmt) => {
      //looks unintuitive, but this means the user will
      // pay 0 network tokens, and pay tokenAmt in rewardTokens
      return [0, [tokenAmt, stakeToken]]
  },
  (tokenAmt, apiReturn) => {
      require(tokenAmt >= 1, 'Must stake at least 1 token');

      userLocalState[this] = {
          stake: tokenAmt,
          pendingRewards: 0,
      }
      // return to the API caller
      apiReturn(true)
  }
)
```

## Unstaking

Unstaking is similar in structure.

``` rsh
.api(User.unstakeAndClaim,
  () => {
      // No rewards left in the contract? Sry!
      assume(balance(rewardToken) > 0, 'No rewards left')
  },
  () => {
      // transfer nothing to the contract
      return [0, [0, stakeToken]]
  },
  (apiReturn) => {
      require(balance(rewardToken) > 0, 'No rewards left')
      const userState = userLocalState[this];
      // pay acrued rewards
      transfer([[userState.pendingrewards, rewardToken]]).to(this);

      userLocalState[this] = {
          stake: 0,
          pendingRewards: 0,
      }
      // return to the API caller
      apiReturn(true)
      // update global state
      return [
          rewardSupply - userState.pendingRewards
      ]
  }
)
```

## Connect to View

Now for the cool part: Use your contract to hook to your website. To accomplish this, we connect to the [contract](@{REPO}/frontend/#js_contract), deine the User's [API](@{REPO}/rsh/appinit/#rsh_API) calls and a [view](@{REPO}/rsh/appinit/#rsh_View)

``` rsh
// connect to the contract
const ctcInfo = account.contract(backend, CONTRACT_INFO);
// defines all User api calls (stakeTokens, unstaketokens)
const userApi = contract.a.User;
// defines the view
const userView = contract.v.read;
```

Next, we get the users state by reading the [address](@{REPO}/model/#term_address).

``` rsh
useEffect(async() => {
    // get users local state from application
    const userState = await userView.read(account.addr);
    setUserState(userState);
}, []);
```

Then create an event listener that connects to the wallet.

``` rsh
<div>
  {/* On button clicks, these functions will trigger the wallet */}
  <button onClick={async () => await userApi.stakeTokens(amt)}>
    Stake Tokens
  </button>

  <button onClick ={async () => await userApi.unstakeTokens(amt)}>
    Unstake Tokens + {userState.pendingRewards} rewards
  </button>
</div>
```

## Conclusion

This demonstrates the essential (but not quite complete) elements of implementing a contract in Reach.
There's no magic involved, but it definitely feels like there is when you're building a Reach application. 
As a developer, you don't need to think about forming transaction groups, SDK calls, or opting in. 
Reach handles `{!rsh} Token` IDs and Application opt-ins, on networks that require opt-ins. You focus on the business logic of your DApp and plug into the consensus network of your choice.

In addition to all of this, Reach also provides formal verification and cross chain compatability. 
In short, Reach allows developers to build decentralized apps on multiple blockchains faster and safer. 