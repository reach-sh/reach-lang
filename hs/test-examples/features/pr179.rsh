"reach 0.1";

const contractDetails = {
  title: Bytes(128),
  instructions: Bytes(512),
  value: UInt,
};

const commonProps = {
  getStatus: Fun([], UInt),
  notifyComplete: Fun([], Null),
  notifyInProgress: Fun([], Null),
  notifyInReview: Fun([], Null),
  updateContract: Fun([], Null),
};

const PrimaryProps = {
  ...commonProps,
  getContractDetails: Fun([], Object(contractDetails)),
};

const ContractorProps = {
  ...commonProps,
  getEvidence: Fun([], Bytes(256)),
};

const ArbitratorProps = {
  ...commonProps,
};

const STATUS = {
  OPEN: 1,
  INPROGRESS: 2,
  REVIEW: 3,
  VERIFIED: 4,
  CANCELED: 5,
};

export const main = Reach.App(() => {
  const Primary = Participant("Primary", PrimaryProps);
  const Contractor = Participant("Contractor", ContractorProps);
  const Arbitrator = Participant("Arbitrator", ArbitratorProps);
  // View
  const vContract = View("vContract", {
    ...contractDetails,
    evidence: Bytes(256),
    status: UInt,
  });

  deploy();

  // 1. Primary publishes contract
  Primary.only(() => {
    const det = declassify(interact.getContractDetails());
    const { instructions, title, value } = det;
    const err = "Offer must be at least 1 ALGO (to cover transaction costs)";
    assume(value > 1, err);

    const contractData = { instructions, title, value };
  });
  Primary.publish(contractData).pay(contractData.value);

  // 1a. Set internal details
  vContract.title.set(contractData.title);
  vContract.instructions.set(contractData.instructions);
  vContract.value.set(contractData.value);
  vContract.status.set(STATUS.INPROGRESS);
  commit();

  // AUX. Create a contract deadline: Number arg represents block rounds.
  const [timeRemaining, keepGoing] = makeDeadline(10);

  Contractor.publish();
  var status = STATUS.INPROGRESS;
  invariant(balance() == contractData.value);
  while (status == STATUS.INPROGRESS) {
    commit();

    Contractor.only(() => {
      const progress = declassify(interact.getStatus());
      const evidence = declassify(interact.getEvidence());
    });
    Contractor.publish(evidence, progress);

    vContract.evidence.set(evidence);
    vContract.status.set(progress);

    each([Primary, Contractor], () => {
      interact.updateContract();
      interact.notifyInProgress();
    });

    status = progress;
    continue;
  }
  // require(status == STATUS.REVIEW, "Invalid status! How did you get here?");
  commit();

  each([Primary, Contractor], () => {
    interact.updateContract();
    interact.notifyInReview();
  });

  // Arbitrator
  Arbitrator.only(() => {
    const review = declassify(interact.getStatus());
  });

  Arbitrator.publish(review)
    .when(keepGoing())
    .timeout(timeRemaining(), () => {
      closeTo(Primary);
    });
  vContract.status.set(review);
  commit();

  // Notify everyone that a review is in, and the contract is ending
  each([Arbitrator, Primary, Contractor], () => {
    interact.notifyComplete();
  });

  Anybody.publish();
  // Payouts: pay contractor if verified; else pay seller
  // (2% arbitrator fee)
  const arbitratorFee = balance() / 50;
  if (review === STATUS.VERIFIED) {
    transfer(balance() - arbitratorFee).to(Contractor);
    commit();
  } else {
    transfer(balance() - arbitratorFee).to(Primary);
    commit();
  }

  closeTo(Arbitrator);
});
