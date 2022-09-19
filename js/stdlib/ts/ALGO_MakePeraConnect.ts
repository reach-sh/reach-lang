
export default function ALGO_MakePeraConnect( PeraWalletConnect:any ) {
  return class PeraConnect {
    pc: any;
    accounts: Array<any>;

    constructor(pc:any = false) {
      this.pc = pc;
      this.accounts = [];
    }

    async ensurePC() {
      if ( this.pc ) { return; }
      this.pc = new PeraWalletConnect();
      this.pc.reconnectSession().then((accts:any) => {
        this.accounts = accts;
      });
    };

    async disconnect() {
      this.pc.disconnect();
      this.accounts = [];
    }

    async ensureSession() {
      await this.ensurePC();
      if ( this.accounts.length === 0 ) {
        this.accounts = await this.pc.connect();
      }
    }

    async getAddr(): Promise<string> {
      await this.ensureSession();
      return this.accounts[0];
    }

    async signTxns(txns:string[]): Promise<string[]> {
      await this.ensureSession();
      return this.pc.signTransaction(
        txns.map((txn) => ({txn}))
      );
    }
  }
};
