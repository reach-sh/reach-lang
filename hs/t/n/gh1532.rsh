'reach 0.1';
const Params = Object({
    companyName: Bytes(128),
    bT: Token,
    lockSale: Bool,
    managerAddr: Address,
    authorizedST: UInt256,
});

export const main = Reach.App(() => {
    setOptions({ untrustworthyMaps: true });

    const currSale = Struct([['qty', UInt], ['price', UInt], ['sold', UInt], ['status', Bytes(5)], ['cumProceeds', UInt]]);
    const currSwap = Object({ qty: UInt, price: UInt, sold: UInt, status: Bytes(5), cumProceeds: UInt });


    const Creator = Participant('Creator', {
        getParams: Fun([Bytes(15)], Params),
        iDeployed: Fun([Bytes(15)], Null),
    })

    const User = API({
        initSwap: Fun([UInt, UInt], currSale)

    });

    const Views = View({
        totSTBTD: Tuple(UInt256, UInt256, UInt256),
        saleLocked: Bool,
        claimSTBT: Fun([Address], Tuple(UInt256, UInt256)),
        totSTBTDRec: Fun([Address], Tuple(UInt256, UInt256, UInt256)),
        wlMember: Fun([Address], Bool),
        wlMembers: Array(Address, 25),
        vHash: Tuple(Bytes(96), UInt, UInt),
        vcVersion: Tuple(Bytes(4), Bytes(36)),
        vBtBal: Tuple(UInt256, Token),
        vCcCm: Tuple(Address, Address),
        vOptedIn: Fun([Address], Bool),
        vCurrSale: currSale,

    });

    init();

    Creator.only(() => {
        const { companyName, lockSale, managerAddr, bT, authorizedST } = declassify(interact.getParams('I have deployed'));
    });

    Creator.publish(companyName, lockSale, managerAddr, bT, authorizedST);

    Creator.interact.iDeployed('I HAVE DEPLOYED');

    const end = UInt.max;
    const unwInt = (m) => fromSome(m, UInt256(0));
    const unwInt64 = (m) => fromSome(m, 0);
    const unwWlMap = (m) => fromSome(m, Creator);
    const unwUintArrMap = (m) => fromSome(m, Array.replicate(100, 0));
    const unwBool = (m) => fromSome(m, false);
    const unwSwapDetails = (m) => fromSome(m, { 'qty': 0, 'price': 0, 'sold': 0, 'status': '-----', 'cumProceeds': 0 });
    const claimST = new Map(UInt256); //share tokens waiting to be claimed by each investor
    const claimBT = new Map(UInt256); //backing tokens waiting to be claimed by each investor
    const totRecST = new Map(UInt256); //total received share token by each investor (NOT USED)
    const totAllST = new Map(UInt256); //total allocated share token for each investor
    const totRecBT = new Map(UInt256); //total received backing token by each investor
    const iDistrNum = new Map(UInt256); //each investor's claimed distribution number
    const swapDetails = new Map(currSwap); //each investor's swap details
    const optedIn = new Map(Bool); //each investor's opted in status
    const wl = new Set(); //whitelist
    const iStHis = new Map(Array(UInt, 100));

    const [state] = parallelReduce([{
        saleDetails: { qty: 0, price: 0, sold: 0, status: 'ended', cumProceeds: 0 },
        wlArr: Array.replicate(25, Creator),
        wlIndex: 0,
        cv: ['1.05', 'cooperativIdcooperativIdcooperativId'],
        currDistr: UInt256(0),
        totST: UInt256(0),
        totBT: UInt256(0),
        saleLocked: lockSale,
        ctcMan: managerAddr,
        distrHis: Array.replicate(100, [0, 0]), //[totST,distrAmount] 
        distrNum: UInt256(0),
        distrIndex: 0,
        docHash: ['ajdnaeinawindiaengtnifrjwritniqwrnirefindinigajdnaeinawidiaengtniitniqwrnirefindinigfuaebfubawur', 0, 0,],
    }])
        .invariant(balance() == 0)
        .while(true)
        .paySpec([bT])
        .define(() => {
            Views.totSTBTD.set([state.totST, state.totBT, state.distrNum]);
            Views.saleLocked.set(state.saleLocked);
            Views.claimSTBT.set((addr) => [unwInt(claimST[addr]), unwInt(claimBT[addr])]);
            Views.totSTBTDRec.set((addr) => [unwInt(totAllST[addr]), unwInt(totRecBT[addr]), unwInt(iDistrNum[addr])]);
            Views.wlMember.set((addr) => wl.member(addr));
            Views.vHash.set(state.docHash);
            Views.vcVersion.set([state.cv[0], state.cv[1]]);
            Views.vBtBal.set([UInt256(balance(bT)), bT]);
            Views.vCcCm.set([Creator, state.ctcMan]);
            Views.vOptedIn.set((addr) => unwBool(optedIn[addr]));
            Views.vCurrSale.set(currSale.fromObject(state.saleDetails));
        }
        ).api(
            User.initSwap,
            // Assumes
            ((qty, price) => {
                assume(wl.member(this), 'you must be a whitelist member');
                assume(qty > 0, 'you must sell at least 1 share token');
                assume(price > 0, 'price must be greater than 0');
                assume(unwSwapDetails(swapDetails[this]).status == '-----', 'there must be no pending swap'); //COME BACK TO THIS
            }),
            // Payments
            ((x, y) => [0, [0, bT]]),
            // Consensus
            ((qty, price, res) => {
                require(wl.member(this), 'you must be a whitelist member');
                require(qty > 0, 'you must sell at least 1 share token');
                require(price > 0, 'price must be greater than 0');
                require(unwSwapDetails(swapDetails[this]).status == '-----', 'there must be no pending swap'); //COME BACK TO THIS
                const swapDet = Object.set(swapDetails[this], 'status', 'initd');
                const swapDet2 = Object.set(swapDet, 'qty', qty);
                const swapDet3 = Object.set(swapDet2, 'price', price);
                swapDetails[this] = swapDet3;
                res(currSale.fromObject(swapDetails[this]));
                return [state];
            })
        )
        .timeout(absoluteTime(end), () => {
            Anybody.publish();
            return [state];
        });

    commit();
    exit();
});