import { loadStdlib } from "@reach-sh/stdlib";
const reach = loadStdlib({
    REACH_CONNECTOR_MODE: 'ALGO', 
})
reach.setProviderByName('TestNet');
// this is a test to prove that the balanceOf function returns values with 
// BigInt precision (18446744073709551615) and not number precision (18446744073709552000)

// to conduct this test it uses a testnet account with a max supply (18446744073709551615) balance 
// of a zero decimal token (DECZ) and a max supply balance of a 19 decimal token (19DEC)

// account page: https://testnet.algoexplorer.io/address/IC5DKGRHEHW3GQIQFJFAYSY3NNVJ7P3LVL3W4X4OLISKJ7224Y4LU5HS4M
const account = await reach.newAccountFromMnemonic('praise candy degree job have detail lonely advice december dolphin convince there burger civil coffee toast celery canal modify feature worry slight notice abandon cook')

// DECZ token page: https://testnet.algoexplorer.io/asset/70474481
const tokenBalanceDECZ = await reach.balanceOf(account, 70474481)
const fmtDECZBalance = reach.formatWithDecimals(tokenBalanceDECZ, 0)
console.log({
  accountDECZBalance: '18446744073709551615',
  accountDECZBalanceFromBalanceOf: fmtDECZBalance,
})
if (fmtDECZBalance !== '18446744073709551615') throw Error('stdlib.balanceOf is not returning balances with accurate precision')

// 19DEC token page: https://testnet.algoexplorer.io/asset/70540863
const tokenBalance19DEC = await reach.balanceOf(account, 70540863)
const fmt19DECBalance = reach.formatWithDecimals(tokenBalance19DEC, 19)
console.log({
    account19DECBalance: '1.8446744073709551615',
    account19DECBalanceFromBalanceOf: fmt19DECBalance,
})
if (fmt19DECBalance !== '1.8446744073709551615') throw Error('stdlib.balanceOf is not returning balances with accurate precision')