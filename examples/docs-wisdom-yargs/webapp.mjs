import * as backend from '/build/index.main.mjs';

let stdlib = null;
let suStr = null;
let iBalance = null;
let sellerAcc = null;
let buyerAcc = null;
let sellerCtc = null;
let buyerCtc = null;

const modal = new bootstrap.Modal(document.getElementById('confirm-modal'), { backdrop: false })

const writeLog = (role, msg) => {
  let el = document.getElementById(`${role}-log`);
  el.append(`${el.value ? '\n' : ''}${msg}`);
  el.scrollTop = el.scrollHeight;
};

const disableBtns = () => {
  const btns = document.querySelectorAll('button.dar').forEach((el) => {
    el.classList.remove('btn-success');
    el.classList.add('btn-secondary');
    el.disabled = true;
  });
};

const enableBtn = (id) => {
  disableBtns();
  const btn = document.getElementById(id);
  btn.classList.remove('btn-secondary');
  btn.classList.add('btn-success');
  btn.removeAttribute('disabled');
};

const toAU = (su) => stdlib.parseCurrency(su);
const toSU = (au) => stdlib.formatCurrency(au, 4);
const showBalance = async (role, acc) => {
  let balance = toSU(await stdlib.balanceOf(acc));
  document.getElementById(`${role}-balance`).value = balance;
};

const commonInteract = (role) => ({
  reportPayment: (payment) => writeLog(role, `${role == 'buyer' ? 'You' : 'The buyer'} paid ${toSU(payment)} ${suStr} to the contract.`),
  reportTransfer: (payment) => writeLog(role, `The contract paid ${toSU(payment)} ${suStr} to ${role == 'seller' ? 'you' : 'the seller'}.`),
  reportCancellation: () => { }
});

// Listener for selecting a devnet
document.getElementById('devnets').addEventListener('change', (event) => {
  reachsdk.unsafeAllowMultipleStdlibs();
  stdlib = reachsdk.loadStdlib(document.getElementById('devnets').value);
  stdlib.setProviderByName('LocalHost');
  suStr = stdlib.standardUnit;
  iBalance = toAU(1000);

  (async () => {
    sellerAcc = await stdlib.newTestAccount(iBalance);
    await showBalance('seller', sellerAcc);
    buyerAcc = await stdlib.newTestAccount(iBalance);
    await showBalance('buyer', buyerAcc);
  })();

  document.getElementById('seller-balance').value = '';
  document.getElementById('seller-log').innerHTML = '';
  document.getElementById('seller-contract-info').value = '';

  document.getElementById('buyer-balance').value = '';
  document.getElementById('buyer-price').value = '';
  document.getElementById('buyer-wisdom').value = '';
  document.getElementById('buyer-log').innerHTML = '';

  document.querySelectorAll('input.unit').forEach((el) => { el.value = suStr; });
  enableBtn('deploy-btn');
});

// Listener for clicking deploy btn
document.getElementById('deploy-btn').addEventListener('click', (event) => {
  (async () => {
    const sellerInteract = {
      ...commonInteract('seller'),
      price: toAU(document.getElementById('seller-price').value),
      wisdom: document.getElementById('seller-wisdom').value,
      reportReady: async (price) => {
        document.getElementById('seller-contract-info').value = JSON.stringify(await sellerCtc.getInfo());
        enableBtn('attach-btn');
      }
    };

    sellerCtc = sellerAcc.contract(backend);
    await backend.Seller(sellerCtc, sellerInteract);
    await showBalance('seller', sellerAcc);
  })();
});

// Listener for clicking attach btn
document.getElementById('attach-btn').addEventListener('click', (event) => {
  (async () => {
    const info = JSON.parse(document.getElementById('seller-contract-info').value);
    buyerCtc = buyerAcc.contract(backend, info);
    const price = await buyerCtc.views.Main.price();
    document.getElementById('buyer-price').value = toSU(price[1]);
    document.querySelector('div.modal-body span').innerHTML = `${toSU(price[1])} ${suStr}`;
    modal.show();
    disableBtns();
  })();
});

// Listener for clicking yes btn
document.getElementById('yes-btn').addEventListener('click', (event) => {
  (async () => {
    const buyerInteract = {
      ...commonInteract('buyer'),
      confirmPurchase: async (price) => true,
      reportWisdom: (wisdom) => { 
        document.getElementById('buyer-wisdom').value = wisdom; 
        enableBtn('reset-btn');
      }
    };

    // await backend.Buyer(buyerCtc, buyerInteract);
    await buyerCtc.p.Buyer(buyerInteract)
    await showBalance('buyer', buyerAcc);
  })();
});

// Listener for clicking no btn
document.querySelectorAll('#no-btn, #close-btn').forEach(el => {
  el.addEventListener('click', (event) => {
    enableBtn('reset-btn');
  });
});

// Listener for clicking reset btn
document.getElementById('reset-btn').addEventListener('click', (event) => {
  document.getElementById('seller-contract-info').value = '';
  document.getElementById('buyer-price').value = '';
  document.getElementById('buyer-wisdom').value = '';
  enableBtn('deploy-btn');
});
