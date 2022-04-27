import type { BaseHTTPClient } from "algosdk";

export type EnableNetworkFunction = (
  opts?: EnableNetworkOpts
) => Promise<EnableNetworkResult>;

export interface EnableNetworkOpts {
  genesisID?: string,   // ascii string
  genesisHash?: string, // base64 string representing a 32-byte genesis hash.
};

export interface EnableNetworkResult {
  genesisID: string,   // ascii string
  genesisHash: string, // base64 string representing a 32-byte genesis hash.
};

export type EnableAccountsFunction = (
  opts?: EnableOpts
) => Promise<EnableAccountsResult>;

export interface EnableAccountsOpts {
  accounts?: string[], // AlgorandAddress[]
};

export interface EnableAccountsResult {
  accounts: string[], // AlgorandAddress[]
};

export type EnableFunction = (
  opts?: EnableOpts
) => Promise<EnableResult>;

export type EnableOpts = EnableNetworkOpts & EnableAccountsOpts;

export type EnableResult = EnableNetworkResult & EnableAccountsResult;

export interface WalletTransaction {
   txn: string,
   signers?: string[]; // AlgorandAddress[]
   message?: string;
   stxn?: string;
};

export type SignTxnsFunction = (
   txns: WalletTransaction[],
   opts?: any,
) => Promise<(string | null)[]>;

export type PostTxnsFunction = (
  stxns: string[], // SignedTxnStr
) => Promise<PostTxnsResult>;

export interface PostTxnsResult {
  txId?: string; // TxId
};

export type SignAndPostTxnsFunction = (
   txns: WalletTransaction[],
   opts?: any,
) => Promise<PostTxnsResult>;

export type GetAlgodv2ClientFunction = () => Promise<BaseHTTPClient>
export type GetIndexerClientFunction = () => Promise<BaseHTTPClient>

export interface WindowAlgorand {
  enable: EnableFunction;
  enableNetwork?: EnableNetworkFunction;
  enableAccounts?: EnableAccountsFunction;
  signTxns?: SignTxnsFunction;
  postTxns?: PostTxnsFunction;
  signAndPostTxns: SignAndPostTxnsFunction;
  getAlgodv2Client: GetAlgodv2ClientFunction;
  getIndexerClient: GetIndexerClientFunction;
};

export type ARC11_Wallet = WindowAlgorand & {
  _env?: any;
};
