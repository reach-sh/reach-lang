export type EnableFunction = (
  opts?: EnableOpts
) => Promise<EnableResult>;

export interface EnableOpts {
  network?: string,    // NetworkIdentifier
  accounts?: string[], // AlgorandAddress[]
};

export interface EnableResult {
  network?: string,   // NetworkIdentifier
  accounts: string[], // AlgorandAddress[]
};

export interface WalletTransaction {
   txn: string,
   signers?: string[]; // AlgorandAddress[]
   message?: string;
   stxn?: string;
};

export type SignTxnFunction = (
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

type Algodv2 = any;
type GetAlgodv2Function = () => Promise<Algodv2>;

type Indexer = any;
type GetIndexerFunction = () => Promise<Indexer>;

export interface WindowAlgorand {
  enable: EnableFunction;
  signAndPostTxns: SignAndPostTxnsFunction;
  getAlgodv2: GetAlgodv2Function;
  getIndexer: GetIndexerFunction;
};

export type ARC11_Wallet = WindowAlgorand;
