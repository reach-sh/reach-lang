'reach 0.1';


// ========================================================
// Helper Functions
// ========================================================

const emptyOrderMetadata = {
  taxAmt: 0,
  shippingAmt: 0,
  unitPrice: 0, 
  orderID: 0,
  orderTotal: 0
}

const setViewActiveOrderMetadata = (view, orderMetadata) => {
  view.taxAmt.set(orderMetadata.taxAmt);
  view.shippingAmt.set(orderMetadata.shippingAmt);
  view.orderID.set(orderMetadata.orderID);
  view.unitPrice.set(orderMetadata.unitPrice);
  view.orderTotal.set(orderMetadata.orderTotal);
}

const resetViewActiveOrderMetadata = (view) => {
  setViewActiveOrderMetadata(view, emptyOrderMetadata);
}
 
const countTrue = (arr) => arr.reduce(0, (z, x) => z + (x ? 1 : 0))


// ========================================================
// Participants
// ========================================================

// This may need to track more info about contract state.
const OrderMetadataInterface = {
  taxAmt: UInt,
  shippingAmt: UInt,
  orderID: UInt,
  unitPrice: UInt,
  orderTotal: UInt
}

const OrderMetadataTypes = [
  UInt, UInt, UInt, UInt, UInt,
]

const MerchantInterface = {
  getTrialData: Fun([], Object({
    initialTrialDurationInDays: UInt,
    trueUnitPrice: UInt,
  })),
  getArbiterAddresses: Fun([], Object({
    feeAddr: Address,
    taxAddr: Address
  })),
  showOrderCreated: Fun([], Null),
  wantsToApproveOrder: Fun([], Bool),
  wantsToDenyOrder: Fun([], Bool),
  markOrderApprovedAndShipped: Fun([], Null)
}

const potentialBuyerErrors = {
  errorDepositHasAlreadyBeenMade: Fun([], Null),
  errorOrderDenied: Fun([], Null)
}

const potentialBuyerAlerts = {
  alertItemDidNotShip: Fun([], Null),
}

const PotentialBuyerInterface = {
  intendsToPurchase: Fun([], Bool),
  getOrderInfo: Fun([], Object(OrderMetadataInterface)),
  showOrderPaid: Fun(OrderMetadataTypes, Null),
  syncToDb: Fun(OrderMetadataTypes, Null),
  ...potentialBuyerErrors,
  ...potentialBuyerAlerts
}

const ViewInterface = {
  ...OrderMetadataInterface
}


// ========================================================
// Main Contract Logic
// ========================================================

export const main = Reach.App(
  {}, [
    Participant('Merchant', MerchantInterface), 
    ParticipantClass('PotentialBuyer', PotentialBuyerInterface),
    View('ActiveOrderMetadata', ViewInterface),
    Participant('HandcraftedFeeAcct', {}),
    Participant('HandcraftedTaxAcct', {}),
  ], (Merchant, PotentialBuyer, vActiveOrderMetadata, HandcraftedFeeAcct, HandcraftedTaxAcct) => {

    Merchant.only(() => {
      const { initialTrialDurationInDays, trueUnitPrice } = declassify(interact.getTrialData());
      const { feeAddr, taxAddr } = declassify(interact.getArbiterAddresses());
      declassify(interact.showOrderCreated());
    });
    Merchant.publish(initialTrialDurationInDays, trueUnitPrice, feeAddr, taxAddr);

    HandcraftedFeeAcct.set(feeAddr);
    HandcraftedTaxAcct.set(taxAddr);

    assert(balance() == 0);

    /**
     * @var {Bool} hasBeenPurchased
     *   If true, the item has been successfully sold and should
     *   therefore no longer be listed as available for purchase,
     *   either in the contract logic or in the frontend/db.
     * @var {OrderMetadataInterface} finalPaymentAmts
     *   The order metadata to be used for final payouts.
     */
    const [ hasBeenPurchased, finalPaymentAmts ] = 
      parallelReduce([ false, emptyOrderMetadata]) 
        .invariant(balance() == finalPaymentAmts.orderTotal)
        .while(!hasBeenPurchased)
        
        // Steps 2 & 3 - Potential buyer makes a deposit.
        .case(PotentialBuyer,
          (() => ({
            when: declassify(interact.intendsToPurchase())
          })),
          ((_) => {

            // We can assume that a deposit has been made if 
            // the balance is greater than 0. It is not enough 
            // to check for a loop var depositIsActive 
            // because that is set at the end of the step, 
            // but a second user may make end up making a deposit
            // between when the first user makes a deposit and
            // the loop returns the updated depositIsActive.
            if(balance() > 0) {
              PotentialBuyer.only(() =>  declassify(interact.errorDepositHasAlreadyBeenMade()));
              return [
                hasBeenPurchased,
                finalPaymentAmts
              ];
            } else {
              const ThisPotentialBuyer = this;
              assert(balance() == 0);
              commit();

              // Buyer publishes supposed order data from the frontend to be paid
              PotentialBuyer.only(() => {
                // We are having the buyer provide the orderTotal from the front end,
                // so we need to have outside validation of this as well.
                const orderMetadata = declassify(interact.getOrderInfo());
                const { taxAmt, shippingAmt, unitPrice, orderID, orderTotal } = orderMetadata;

                declassify(interact.showOrderPaid(taxAmt, shippingAmt, unitPrice, orderID, orderTotal));
                // This needs to be fleshed out.
                declassify(interact.syncToDb(taxAmt, shippingAmt, unitPrice, orderID, orderTotal));
              });
              PotentialBuyer.publish(orderMetadata)
                .pay(orderMetadata.orderTotal);


              assert(balance() == orderMetadata.orderTotal);
              setViewActiveOrderMetadata(vActiveOrderMetadata, orderMetadata);


              // 4. Approval/refund logic
              const [ orderTimedOut, orderDenied, orderApproved ] = 
                parallelReduce([ false, false, false ])
                  .invariant((countTrue(array(Bool, [orderTimedOut, orderDenied, orderApproved])) <= 1)    
                    && (balance() == orderMetadata.orderTotal))
                  .while(!orderTimedOut && !orderDenied && !orderApproved)
                  .case(
                    Merchant,
                    (() => ({ when: declassify(interact.wantsToApproveOrder())})),
                    () => { return [ false, false, true ]; }
                  )                  
                  // Commented out until fix is released: https://github.com/reach-sh/reach-lang/issues/205#issuecomment-873118085
                  // .case(
                  //   Merchant,
                  //   (() => ({ when: declassify(interact.wantsToDenyOrder())})),
                  //   () => { return [ false, true, false ]; }
                  // )
                  //
                  // Test case, not true timeout.
                  .timeout(1000, () => {
                    Anybody.publish();
                    return [ false, false, true ];
                  });  

              // Verification fails without this. Seems like it's needed
              // to protect against some sort of malicious activity in 
              // the timeout due to Anybody.publish().
              if(balance() != orderMetadata.orderTotal) {
                transfer(balance()).to(ThisPotentialBuyer);
                return [ false, emptyOrderMetadata];
              } else {
                return [ true, orderMetadata ];
              }
            }
          })
        ) 
        .timeout(false);

    commit();
    Anybody.publish();
    transfer(finalPaymentAmts.orderTotal).to(Merchant);
    commit();
    exit();
  }
);


