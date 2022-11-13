export default function ALGO_MakeAlgoSignerConnect( AlgoSigner: any, provider: string ) {
    return class AlgoSignerConnect {
        constructor() {}

        async connect() {
            await AlgoSigner.connect();
            return await AlgoSigner.accounts({ledger:provider});
        }

        async signTransaction(txns: any) {
            let ntxns: any = [];
            txns.forEach((x: any) => {
                ntxns.push({txn: x});
            });
            let stxns = await AlgoSigner.signTxn(ntxns);

            const result: any = [];
            stxns.forEach((t: any) => {
                result.push({
                    blob: Buffer.from(t.blob, 'base64'),
                    txID: t.txID
                });
            });

            return result;
        }
    }
}
