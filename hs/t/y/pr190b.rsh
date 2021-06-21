"reach 0.1";

// ========================================================
// Constants
// ========================================================

const BAKESALE_FEE_PERCENT = 5; 
const BAKESALE_FEE_AS_DECIMAL = BAKESALE_FEE_PERCENT / 100;

const MAX_BENEFICIARIES_PER_ITEM = 1;
const MAX_LINE_ITEMS_PER_MERCHANT = 1;
const MAX_TOTAL_MERCHANTS = 1;


// ========================================================
// Primary Objects & Their Fakers
// ========================================================

/**
 * The beneficiary of a particular item.
 */
const Beneficiary = {
  isReal: Bool,
  addr: Address,
  percentToReceive: UInt,
};

/**
 * Create a fake benficiary.
 * 
 * @param {Address} addr 
 *   The fake address. 
 * @returns {Beneficiary}
 *   The fake Beneficiary.
 */
const createFakeBeneficiary = (addr) => ({
  isReal: false,
  addr,
  percentToReceive: 0
})

/**
 * An order line item.
 */
const LineItem = {
  isReal: Bool,
  unitPrice: UInt,
  qty: UInt,
  shipping: UInt,
  tax: UInt,
  beneficiaries: Array(Maybe(Object(Beneficiary)), MAX_BENEFICIARIES_PER_ITEM)
}

/**
 * Create a fake LineItem.
 * 
 * @param {Address} addr 
 *   The fake address.
 * @returns {LineItem}
 *   The fake LineItem.
 */
const createFakeLineItem = (addr) => ({
  isReal: false,
  unitPrice: 0,
  qty: 0,
  shipping: 0,
  tax: 0,
  beneficiaries: Array.iota(MAX_BENEFICIARIES_PER_ITEM).map(_ =>  
    Maybe(Object(Beneficiary)).Some(createFakeBeneficiary(addr)))
})

/**
 * Array of order line items for a particular merchant.
 */
const Merchant = {
  isReal: Bool,
  addr: Address,
  lineItems: Array(Maybe(Object(LineItem)), MAX_LINE_ITEMS_PER_MERCHANT) 
}

/**
 * Create a fake Merchant.
 * 
 * @param {Address} addr 
 *   The fake address. 
 * @returns {Merchant}
 *   The fake Merchants obj.
 */
const createFakeMerchant = (addr) => ({ 
  isReal: false,
  addr,
  lineItems: Array.iota(MAX_LINE_ITEMS_PER_MERCHANT).map(_ => 
    Maybe(Object(LineItem)).Some(createFakeLineItem(addr)))
});

/**
 * The order data for which payment is processed.
 */
const OrderData = {
  merchants: Array(Maybe(Object(Merchant)), MAX_TOTAL_MERCHANTS),
}


// ========================================================
// Laundromat
// ========================================================

/**
 * Convert Array(Maybe(Object(Foo)) to usable Array(Object(Foo)).
 * 
 * @param {Array(Maybe(Object(Foo))} arr
 *   The array of maybe els.
 * @param {Address} addr 
 *   The address to be used for the fakes.
 * @param {Function} callbackCreateFaker
 *   The callback to create a fake for array Foo type.
 * @returns {Array(Object(Foo)}
 *   Array of operable Foo els.
 */
const cleanMaybeArr = (arr, addr, callbackCreateFaker) => {
  return arr.map((x) => {
    return fromMaybe(x,  
      (() => callbackCreateFaker(addr)), 
      ((y) => ({isReal: true, ...y}))
    );  
  })
}

/**
 * Convert Array(Maybe(Object(Beneficiary)) to usable Array(Object(Beneficiary)).
 * 
 * @param {Array(Maybe(Object(Beneficiary))} b
 *   The array of maybe beneficiaries.
 * @param {Address} addr 
 *   The address to be used for the fakes.
 * @returns {Array(Object(Beneficiary)}
 *   Array of usable beneficiaries.
 */
 const cleanBeneficiaries = (b, addr) => cleanMaybeArr(b, addr, createFakeBeneficiary);

/**
 * Convert Array(Maybe(Object(LineItem)) to usable Array(Object(LineItem)).
 * 
 * @param {Array(Maybe(Object(LineItem))} lis
 *   The array of maybe line items.
 * @param {Address} addr 
 *   The address to be used for the fakes.
 * @returns {Array(Object(LineItem)}
 *   Array of usable line items.
 */
const cleanLineItems = (lis, addr) => cleanMaybeArr(lis, addr, createFakeLineItem);

/**
 * Convert Array(Maybe(Object(Merchant)) to usable Array(Object(Merchant)).
 * 
 * @param {Array(Maybe(Object(Merchant))} m
 *   The array of maybe merchants.
 * @param {Address} addr 
 *   The address to be used for the fakes.
 * @returns {Array(Object(Merchant)} 
 *   Array of usable merchants.
 */
const cleanMerchants = (m, addr) => cleanMaybeArr(m, addr, createFakeMerchant);

/**
 * Clean the entire orderData's tree of Maybes.
 * 
 * @param {OrderData} orderData
 *   The order data to be cleaned. 
 * @param {Address} fakesAddr 
 *   The address to be used for fakes.
 * 
 * @returns {OrderData} 
 *   The cleaned OrderData. 
 */
const cleanOrderData = (orderData, fakesAddr) => {
  const merchantsClean = cleanMerchants(orderData.merchants, fakesAddr);
  const merchantsFinal = merchantsClean.map(x => {
    
    // Cleaning LineItems for single Merchant 
    const lineItemsClean = cleanLineItems(x.lineItems, fakesAddr);
    const lineItemsFinal = lineItemsClean.map(y => {
      
      // Cleaning Benficiaries for a single LineItem 
      const beneficiariesClean = cleanBeneficiaries(y.beneficiaries, fakesAddr);

      return {
        isReal: y.isReal,
        unitPrice: y.unitPrice,
        qty: y.qty,
        shipping: y.shipping,
        tax: y.tax,
        beneficiaries: beneficiariesClean
      }
    });
     
    return { 
      isReal: x.isReal,
      addr: x.addr,
      lineItems: lineItemsFinal
    }
  }); 

  return {
    merchants: merchantsFinal 
  }
}


// ========================================================
// Order Validation
// ========================================================

/**
 * Calculate the order total.
 * 
 * @param {OrderData} orderData
 *   The CLEANED order data. The logic assumes that all
 *   objects are operable, i.e. no Maybes.
 * 
 * @return {UInt} 
 *   The order total.
 */
const getOrderTotal = (orderData) => {
  const merchantTotals = orderData.merchants.map(m => {
    const liTotals = m.lineItems.map(li => (li.unitPrice * li.qty) + li.shipping + li.tax);
    return liTotals.sum(); 
  });

  return merchantTotals.sum();
}

/**
 * Check whether or not the order is valid to be processed.
 * 
 * @todo Check that no merchants appear more than once
 *   since this might be an indication of double-charging.
 * 
 * @param {OrderData} orderData
 *   The CLEANED order data. The logic assumes that all
 *   objects are operable, i.e. no Maybes.
 */
const validateCleanOrder = (orderData) => {
  // Unlikely that an orderTotal would be <= 0 given the below
  // validation of the amts and pcts below, but keeping this
  // for the sake of sanity.
  const orderTotal = getOrderTotal(orderData);
  const positiveOrderTotal = orderTotal > 0;

  // All amounts on the order must be >= 0.
  const allAmtsNonNegative = orderData.merchants.all(m => {
    const itemsNonNegative = m.lineItems.all(li => {
      const unitPriceValid = li.isReal ? li.unitPrice > 0 : li.unitPrice == 0
      const qtyValid = li.isReal ? li.qty > 0 : li.qty == 0
      const shippingValid = li.isReal ? li.shipping >= 0 : li.shipping == 0
      const taxValid = li.isReal ? li.tax >= 0 : li.tax == 0
      return unitPriceValid && qtyValid && shippingValid && taxValid;
    });
 
    return itemsNonNegative;
  });

  // Any individual item's beneficiary percentages need to be <= 100
  const beneficiaryPctsValid = orderData.merchants.all(m => {
    const itemPctsValid = m.lineItems.all(li => {
      const totalBensPct = li.beneficiaries.map(b => b.percentToReceive).sum();
      const realBensPctPos = li.beneficiaries.all(b => 
        b.isReal ? b.percentToReceive > 0 : b.percentToReceive == 0);
      
      return totalBensPct <= 100 && realBensPctPos;
    }); 
 
    return itemPctsValid;
  });

  const orderIsValid = positiveOrderTotal && allAmtsNonNegative && beneficiaryPctsValid;

  return {
    orderIsValid,
    positiveOrderTotal,
    allAmtsNonNegative,
    beneficiaryPctsValid,
  }
}


// ========================================================
// Participant Info
// ========================================================

/**
 * Errors to present to the buyer on validation or other failure.
 */
 const errors = {
  // errorInvalidOrderTotal: Fun([], Null),
  // errorAmountsExceedBalance: Fun([], Null),
  // errorInvalidRecipientAmounts: Fun([], Null),
  errorGenericInvalidOrder: Fun([], Null),
}
 
/**
 * The Buyer Participant Interface.
 */
const BuyerInterface = {  
  getOrderData: Fun([], Object(OrderData)),
  // alertPaidRecipient: Fun([Address, UInt, Bytes], Null),
  ...errors,
};
 

// ========================================================
// Main Contract Logic
// ========================================================

export const main = Reach.App(
  {}, [ 
    ParticipantClass('Buyer', BuyerInterface),
    Participant('Bakesale', {}),
  ],
  (Buyer, Bakesale) => {
    Bakesale.publish();

    const keepGoing = 
      parallelReduce(true)
        .invariant(balance() == balance())
        .while(keepGoing)
        .case(Buyer,
          (() => {
            const orderData = declassify(interact.getOrderData());
            const orderDataClean = cleanOrderData(orderData, Bakesale);
            const validationResult = validateCleanOrder(orderDataClean);

            if(!validationResult.orderIsValid) {
              interact.errorGenericInvalidOrder();
            } 
            return {
              when: validationResult.orderIsValid
            }
          }), 
          ((_) => {
            commit();
            Buyer.only(() => {
              const orderData = declassify(interact.getOrderData());
            });
            Buyer.publish(orderData); 
            const orderDataClean = cleanOrderData(orderData, Bakesale);
            const validationResult = validateCleanOrder(orderDataClean);

            // This should never happen since we validate in the 
            // local step above, but just in case.
            if(!validationResult.orderIsValid) { 
              Buyer.only(() => declassify(interact.errorGenericInvalidOrder()));
              return true;
            } else {
              const orderTotal = getOrderTotal(orderDataClean);
              commit();
              Buyer.publish().pay(orderTotal);

              // ========================================================
              // Payout all parties.
              // ========================================================
              // Keep this all simple for MVP testing.
              // We can make the transfers more efficient
              // and aggregated after we know it works. 
              orderDataClean.merchants.forEach(merchant => {

                // ========================================================
                // Looking at a single merchant. 
                // ========================================================
                if(merchant.isReal) {
                  merchant.lineItems.forEach(li => {

                    // ========================================================
                    // Looking at a single line item.
                    // ========================================================
                    if(li.isReal) {
                      const liUnitsCost = li.unitPrice * li.qty;
                      const thisLiTotal = liUnitsCost + li.tax + li.shipping;

                      const serviceFee = liUnitsCost * BAKESALE_FEE_AS_DECIMAL;
                      assert(serviceFee < liUnitsCost);

                      // The amount that qualifies for merchant/benficiary payout,
                      const feedLiUnitsCost = liUnitsCost - serviceFee;

                      // ========================================================
                      // Set up payments for final assertion and transfer.
                      // ======================================================== 
                      const pctToBeneficiaries = li.beneficiaries.map(b => b.percentToReceive).sum();
                      const pctToBeneficiariesAsDecimal = pctToBeneficiaries / 100;
                      const amtToBeneficiaries = feedLiUnitsCost * pctToBeneficiariesAsDecimal;

                      const pctToMerchantAsDecimal = 1 - pctToBeneficiariesAsDecimal
                      const feedLiMinusBensPct = pctToMerchantAsDecimal * feedLiUnitsCost;
                      const amtToMerchant = feedLiMinusBensPct + li.shipping;

                      const amtBeingPaidOnLi = serviceFee + amtToMerchant + amtToBeneficiaries + li.tax;
                      assert(amtToBeneficiaries + amtToMerchant == feedLiUnitsCost + li.shipping);
                      assert(amtBeingPaidOnLi == thisLiTotal);

                      // ========================================================
                      // Execute all transfers
                      // ========================================================
                      transfer(serviceFee).to(Bakesale);
                      // Buyer.interact(declassify(interact.alertPaidRecipient(Bakesale, serviceFee, 'service fee')))

                      /** @todo This needs to go to a tax acct. */
                      transfer(li.tax).to(Bakesale)
                      transfer(amtToMerchant).to(merchant.addr);

                      // Pay out the beneficiaries 
                      li.beneficiaries.forEach(b => {
                        if(b.isReal) {
                          assert(b.percentToReceive > 0);
                          const pctAsDecimal = b.percentToReceive / 100;
                          const amt = feedLiUnitsCost * pctAsDecimal;
                          transfer(amt).to(b.addr);
                        }
                      })
                    }
                  })
                }
              });

              return true;
            } 
          })
        )
        // Sufficiently long timeout that it will never be
        // executed since the contract needs to be persistent.
        .timeout(100^100, () => {
          Anybody.publish();
          return false;
        }); 
    
    if(balance() > 0) {
      // Not sure how we'd ever end up with a balance here, 
      // but for now just transfer the remaining balance.
      // We need to fix this so that it tracks the last buyer
      // since it seems like they'd be the most likely source 
      // of these orphan funds?
      transfer(balance()).to(Bakesale);
    }

    commit();
  }
);



