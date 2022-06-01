'reach 0.1';

export const main = Reach.App(() => {
    const Insurer = Participant('Insurer', {
        mandatoryEntryFee: UInt,
        communityGroupName: Bytes(60),
        contractIsRunning: Bool,
        //approveNewMembership: Fun([Address], Null),
        createInvoices: Fun([], Null),
        moveMaturedPayments: Fun([], Null),
        saveNewMemberDetails: Fun([Struct([
            ["fullName", Bytes(60)], ["phone", Bytes(20)],
            ["email", Bytes(60)], ["chosenInsurancePackage", UInt]
        ])], Null),
        saveNewClaim: Fun([Struct([
            ["amountRequested", UInt], ["description", Bytes(200)]
        ])], Null),
        notifyMembersAboutNewClaim: Fun([Struct([
            ["ownerAddr", Address],
            ["amountRequested", UInt],
            ["description", Bytes(600)],
            ["supportDocuments", Bytes(100)]
        ])], Null),
        seeFeedback: Fun([], Null),
        signout: Fun([], Null),
        notifyFundedMember: Fun([Address], Null),
        stopContract: Fun([], Null),
        log: Fun(true, Null) //REF: https://docs.reach.sh/guide/logging/
    });

    const CommunityMember = API('CommunityMember', {
        registerMembership: Fun([Struct([
            ["fullName", Bytes(60)], ["phone", Bytes(20)],
            ["email", Bytes(60)],
            ["chosenInsurancePackage", UInt]
        ])], Bool),
        payMonthlyFee: Fun([Struct([["who", Address], ["mfee", UInt]])], Bool),
        createClaim: Fun([Struct([
            ["amountRequested", UInt], ["amountSet", UInt], ["accepted", Bool],
            ["approvalsCount", UInt], ["sumOfSetAmounts", UInt],
            ["insrPackageId", UInt], ["amountDue", UInt], ["matureBalance", UInt],
            ["fundLimit", UInt], ["description", Bytes(200)]
        ])], Bool),
        respondToClaim: Fun([Struct([
           ["claimant", Address], ["accepted", Bool], ["setAmount", UInt]
        ])], Bool),
        withDrawClaim: Fun([], Bool),
        //changePackage: Fun([Bytes(60)], Bool),
        stopContract: Fun([], Bool)
    });
    setOptions({ untrustworthyMaps: true });
    init();


    //REF: REACH ARCHITECTURE: https://docs.reach.sh/rsh/#ref-programs

    Insurer.only(() => {
        const mandatoryEntryFee = declassify(interact.mandatoryEntryFee);
        const contractIsRunning = declassify(interact.contractIsRunning);
        interact.seeFeedback();
    });
    Insurer.publish(mandatoryEntryFee, contractIsRunning);
    const invariantCondition = true;
    Insurer.interact.log("backend: starting...");
    commit();
    Insurer.publish();

    //keep a list of all members' Addresses,
    const registeredMembers = new Set();

    const insuranceClaims = new Map(Struct([
      ["amountRequested", UInt], ["amountSet", UInt], ["accepted", Bool],
      ["approvalsCount", UInt], ["sumOfSetAmounts", UInt]
    ]));

    const claimOwners = new Map(Struct([
        ["insrPackageId", UInt],
        ["amountDue", UInt],
      ["matureBalance", UInt]
    ]));

    const [
        membersCount,
        claimsCount
    ] = parallelReduce([1, 1])
        .define(() => {
            const readFromMap = (key) => {
                return (objectInMapInstance) => {
                    return objectInMapInstance[key];
                };
            };
        })
        .invariant(invariantCondition)
        .while(contractIsRunning)
        .api(CommunityMember.registerMembership,
            (_) => { const _ = true; },
            (_) => mandatoryEntryFee,
            ((newMemberDetails, sendResponse) => {
                const who = this;
                sendResponse(true);
                Insurer.interact.log("backend: API.CommunityMember.registerMembership ...");
                Insurer.interact.log("backend: Insurer.interact.saveNewMemberDetails invoked ...");
                Insurer.interact.saveNewMemberDetails(newMemberDetails);
                Insurer.interact.log("backend: done.");
                transfer(mandatoryEntryFee).to(Insurer);

                //add member's address to the list of addresses
                registeredMembers.insert(who);

                return [membersCount + 1, claimsCount];
            })
        ).api(CommunityMember.payMonthlyFee,
            (ob) => { const _ = true; },
            (ob) => ob.mfee,
            ((ob, sendResponse) => {
                Insurer.interact.log("backend: API.CommunityMember.payMonthlyFee invoked.");
                sendResponse(true);
                //deposit into the treasury account
                transfer(ob.mfee).to(Insurer);
                return [membersCount, claimsCount];
            })
        ).api(CommunityMember.createClaim,
            (_) => { const _ = true; },
            (_) => 0,
            ((claimInfo, sendResponse) => {
                const who = this;
                Insurer.interact.saveNewClaim(Struct([["amountRequested", UInt], ["description", Bytes(200)]
                ]).fromObject({amountRequested: claimInfo.amountRequested, description: claimInfo.description }));
                Insurer.interact.log("backend: API.CommunityMember.createClaim ...");

                //add the details to the map of current claim owners
                claimOwners[who] = Struct([["insrPackageId", UInt], ["amountDue", UInt], ["matureBalance", UInt]]).fromObject({
                    insrPackageId: claimInfo.insrPackageId,
                    amountDue: claimInfo.amountDue,
                    matureBalance: claimInfo.matureBalance
                });

                const fundLimitt = claimInfo.fundLimit;

                const amt = claimInfo.amountRequested;
                const claimAmount = amt >= fundLimitt ? amt : fundLimitt;
                insuranceClaims[who] = Struct([["amountRequested", UInt], ["amountSet", UInt], ["accepted", Bool], ["approvalsCount", UInt], ["sumOfSetAmounts", UInt]]).fromObject({
                    amountRequested: amt,
                    amountSet: claimAmount, 
                    accepted: false, approvalsCount: 0,
                    sumOfSetAmounts: amt
                })
                sendResponse(true);

                //change mode from "concensus step" to "step"
                commit();
                
                //now change mode back to "concensus step" and pay for the claimant.
                Insurer.pay(claimAmount);

                return [membersCount, claimsCount + 1];
            })
        ).api(CommunityMember.respondToClaim,
            (_) => { const _ = true; },
            (_) => 0,
            ((opinion, sendResponse) => {
                const who = this;
                const forWho = opinion.claimant;
                sendResponse(true);
                Insurer.interact.log("backend: API.CommunityMember.respondToClaim ...");
                if (opinion.accepted) {
                    const approvalsCnt = maybe(insuranceClaims[forWho], 1, readFromMap("approvalsCount"));
                    const sumOfSetAmts = maybe(insuranceClaims[forWho], 0, readFromMap("sumOfSetAmounts"));
                    const amtRqsted = maybe(insuranceClaims[forWho], 0, readFromMap("amountRequested"));
                    const amtSet = maybe(insuranceClaims[forWho], amtRqsted, readFromMap("amountSet"));
                    const agreedClaimAmount = (approvalsCnt < 5) ? amtSet : sumOfSetAmts / approvalsCnt;
                    insuranceClaims[forWho] = Struct([["amountRequested", UInt], ["amountSet", UInt], ["accepted", Bool], ["approvalsCount", UInt], ["sumOfSetAmounts", UInt]]).fromObject({
                        approvalsCount: approvalsCnt + 1,
                        amountSet: agreedClaimAmount,
                        accepted: true,
                        amountRequested: amtRqsted,
                        sumOfSetAmounts: sumOfSetAmts
                    });

                    if (approvalsCnt >= 5) {
                        //transfer(agreedClaimAmount).to(forWho);

                        //eliminate the member from the list of claim owners (membersWithClaims)
                        delete claimOwners[forWho];

                        //eliminate the claim from the list of open claims (openClaims)
                        delete insuranceClaims[forWho];

                        //Notify the funded member
                        Insurer.interact.notifyFundedMember(forWho);
                    }
                }

                return [membersCount, claimsCount];
            })
        ).api(CommunityMember.withDrawClaim,
            () => { const _ = true; },
            () => 0,
            (sendResponse) => {
                const who = this;
                sendResponse(true);

                //take the funds that the insurer had put on the table (paid), back into the treasury
                const memberRequestedAmount = maybe(insuranceClaims[who], 0, readFromMap("amountRequested"));
                //transfer(memberRequestedAmount).to(Insurer);

                //delete the claim from the list of open claims
                delete insuranceClaims[who];

                //eliminate this member from the list of claim owners (membersWithClaims)
                delete claimOwners[who];

                return [membersCount, claimsCount];
            }
        ).api(CommunityMember.stopContract,
            () => { const _ = true; },
            () => 0,
            ((sendResponse) => {
                //this must be done by the deployer of the contract only.
                const who = this;
                //TODO: require(addressOf(who) == addressOf(Insurer), "You are not allowed to take this action.");
                Insurer.interact.stopContract();
                
                //send response to the API caller (ie, community member participant)
                sendResponse(true);

                return [membersCount, claimsCount];
            })
        );

    //cleanup: send all pending funds to the insurer (if any)
    transfer(balance()).to(Insurer);

    //terminate the concensus step
    commit();

    exit();
});

//REF:
//follow example:  https://github.com/reach-sh/reach-lang/blob/master/examples/rsvp/index.rsh
//invariants info: https://en.wikipedia.org/wiki/Loop_invariant
//TODO: use IPFS:  https://www.freecodecamp.org/news/technical-guide-to-ipfs-decentralized-storage-of-web3/
//to use supabase: https://supabase.com/docs/reference/javascript/delete


