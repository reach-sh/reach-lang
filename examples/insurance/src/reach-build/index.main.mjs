// Automatically generated with Reach 0.1.10 (c0bba7d2)
/* eslint-disable */
export const _version = '0.1.10';
export const _versionHash = '0.1.10 (c0bba7d2)';
export const _backendVersion = 15;

export function getExports(s) {
  const stdlib = s.reachStdlib;
  return {
    };
  };
export function _getEvents(s) {
  const stdlib = s.reachStdlib;
  return {
    };
  };
export function _getViews(s, viewlib) {
  const stdlib = s.reachStdlib;
  const ctc0 = stdlib.T_Address;
  const ctc1 = stdlib.T_UInt;
  const ctc2 = stdlib.T_Bool;
  const ctc3 = stdlib.T_Null;
  const ctc4 = stdlib.T_Data({
    None: ctc3,
    Some: ctc3
    });
  const ctc5 = stdlib.T_Struct([['amountRequested', ctc1], ['amountSet', ctc1], ['accepted', ctc2], ['approvalsCount', ctc1], ['sumOfSetAmounts', ctc1]]);
  const ctc6 = stdlib.T_Data({
    None: ctc3,
    Some: ctc5
    });
  const ctc7 = stdlib.T_Struct([['insrPackageId', ctc1], ['amountDue', ctc1], ['matureBalance', ctc1]]);
  const ctc8 = stdlib.T_Data({
    None: ctc3,
    Some: ctc7
    });
  const map0_ctc = ctc4;
  
  const map1_ctc = ctc6;
  
  const map2_ctc = ctc8;
  
  
  return {
    infos: {
      },
    views: {
      1: [ctc0, ctc1, ctc2],
      4: [ctc0, ctc1, ctc2, ctc1],
      5: [ctc0, ctc1, ctc2, ctc1, ctc1]
      }
    };
  
  };
export function _getMaps(s) {
  const stdlib = s.reachStdlib;
  const ctc0 = stdlib.T_Null;
  const ctc1 = stdlib.T_Data({
    None: ctc0,
    Some: ctc0
    });
  const ctc2 = stdlib.T_UInt;
  const ctc3 = stdlib.T_Bool;
  const ctc4 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2]]);
  const ctc5 = stdlib.T_Data({
    None: ctc0,
    Some: ctc4
    });
  const ctc6 = stdlib.T_Struct([['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2]]);
  const ctc7 = stdlib.T_Data({
    None: ctc0,
    Some: ctc6
    });
  const ctc8 = stdlib.T_Tuple([ctc1, ctc5, ctc7]);
  return {
    mapDataTy: ctc8
    };
  };
export async function _CommunityMember_createClaim4(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for _CommunityMember_createClaim4 expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for _CommunityMember_createClaim4 expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Null;
  const ctc1 = stdlib.T_Data({
    None: ctc0,
    Some: ctc0
    });
  const ctc2 = stdlib.T_UInt;
  const ctc3 = stdlib.T_Bool;
  const ctc4 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2]]);
  const ctc5 = stdlib.T_Data({
    None: ctc0,
    Some: ctc4
    });
  const ctc6 = stdlib.T_Struct([['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2]]);
  const ctc7 = stdlib.T_Data({
    None: ctc0,
    Some: ctc6
    });
  const ctc8 = stdlib.T_Address;
  const ctc9 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '200'));
  const ctc10 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2], ['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2], ['fundLimit', ctc2], ['description', ctc9]]);
  const ctc11 = stdlib.T_Tuple([ctc10]);
  const ctc12 = stdlib.T_Struct([['who', ctc8], ['mfee', ctc2]]);
  const ctc13 = stdlib.T_Tuple([ctc12]);
  const ctc14 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '60'));
  const ctc15 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '20'));
  const ctc16 = stdlib.T_Struct([['fullName', ctc14], ['phone', ctc15], ['email', ctc14], ['chosenInsurancePackage', ctc2]]);
  const ctc17 = stdlib.T_Tuple([ctc16]);
  const ctc18 = stdlib.T_Struct([['claimant', ctc8], ['accepted', ctc3], ['setAmount', ctc2]]);
  const ctc19 = stdlib.T_Tuple([ctc18]);
  const ctc20 = stdlib.T_Tuple([]);
  const ctc21 = stdlib.T_Data({
    CommunityMember_createClaim0_53: ctc11,
    CommunityMember_payMonthlyFee0_53: ctc13,
    CommunityMember_registerMembership0_53: ctc17,
    CommunityMember_respondToClaim0_53: ctc19,
    CommunityMember_stopContract0_53: ctc20,
    CommunityMember_withDrawClaim0_53: ctc20
    });
  
  const map0_ctc = ctc1;
  const map0 = stdlib.newMap({
    ctc: ctc,
    idx: 0,
    isAPI: true,
    ty: map0_ctc
    });
  
  const map1_ctc = ctc5;
  const map1 = stdlib.newMap({
    ctc: ctc,
    idx: 1,
    isAPI: true,
    ty: map1_ctc
    });
  
  const map2_ctc = ctc7;
  const map2 = stdlib.newMap({
    ctc: ctc,
    idx: 2,
    isAPI: true,
    ty: map2_ctc
    });
  
  
  const [v558, v559, v560, v578] = await ctc.getState(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '4'), [ctc8, ctc2, ctc3, ctc2]);
  const v615 = stdlib.protect(ctc11, await interact.in(), {
    at: './index.rsh:1:23:application',
    fs: ['at ./index.rsh:145:17:application call to [unknown function] (defined at: ./index.rsh:145:17:function exp)', 'at ./index.rsh:95:23:application call to "runCommunityMember_createClaim0_53" (defined at: ./index.rsh:144:14:function exp)', 'at ./index.rsh:95:23:application call to [unknown function] (defined at: ./index.rsh:95:23:function exp)'],
    msg: 'in',
    who: 'CommunityMember_createClaim'
    });
  const v642 = ['CommunityMember_createClaim0_53', v615];
  
  const txn1 = await (ctc.sendrecv({
    args: [v558, v559, v560, v578, v642],
    evt_cnt: 1,
    funcNum: 3,
    lct: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc21],
    pay: [stdlib.checkedBigNumberify('./index.rsh:146:20:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      stdlib.simMapDupe(sim_r, 0, map0);
      stdlib.simMapDupe(sim_r, 1, map1);
      stdlib.simMapDupe(sim_r, 2, map2);
      
      const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
      
      switch (v689[0]) {
        case 'CommunityMember_createClaim0_53': {
          const v692 = v689[1];
          sim_r.txns.push({
            kind: 'api',
            who: "CommunityMember_createClaim"
            });
          ;
          const v710 = v692[stdlib.checkedBigNumberify('./index.rsh:144:14:spread', stdlib.UInt_max, '0')];
          const v711 = v710.amountRequested;
          const v717 = v710.insrPackageId;
          const v718 = v710.amountDue;
          const v719 = v710.matureBalance;
          const v720 = {
            amountDue: v718,
            insrPackageId: v717,
            matureBalance: v719
            };
          await stdlib.simMapSet(sim_r, 2, v688, v720);
          const v721 = v710.fundLimit;
          const v723 = stdlib.ge(v711, v721);
          const v724 = v723 ? v711 : v721;
          const v725 = {
            accepted: false,
            amountRequested: v711,
            amountSet: v724,
            approvalsCount: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
            sumOfSetAmounts: v711
            };
          await stdlib.simMapSet(sim_r, 1, v688, v725);
          const v726 = true;
          const v727 = await txn1.getOutput('CommunityMember_createClaim', 'v726', ctc3, v726);
          
          sim_r.isHalt = false;
          
          break;
          }
        case 'CommunityMember_payMonthlyFee0_53': {
          const v870 = v689[1];
          
          break;
          }
        case 'CommunityMember_registerMembership0_53': {
          const v1048 = v689[1];
          
          break;
          }
        case 'CommunityMember_respondToClaim0_53': {
          const v1226 = v689[1];
          
          break;
          }
        case 'CommunityMember_stopContract0_53': {
          const v1404 = v689[1];
          
          break;
          }
        case 'CommunityMember_withDrawClaim0_53': {
          const v1582 = v689[1];
          
          break;
          }
        }
      return sim_r;
      }),
    soloSend: false,
    timeoutAt: undefined /* mto */,
    tys: [ctc8, ctc2, ctc3, ctc2, ctc21],
    waitIfNotPresent: false
    }));
  const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
  switch (v689[0]) {
    case 'CommunityMember_createClaim0_53': {
      const v692 = v689[1];
      undefined /* setApiDetails */;
      ;
      const v710 = v692[stdlib.checkedBigNumberify('./index.rsh:144:14:spread', stdlib.UInt_max, '0')];
      const v711 = v710.amountRequested;
      const v717 = v710.insrPackageId;
      const v718 = v710.amountDue;
      const v719 = v710.matureBalance;
      const v720 = {
        amountDue: v718,
        insrPackageId: v717,
        matureBalance: v719
        };
      await stdlib.mapSet(map2, v688, v720);
      const v721 = v710.fundLimit;
      const v723 = stdlib.ge(v711, v721);
      const v724 = v723 ? v711 : v721;
      const v725 = {
        accepted: false,
        amountRequested: v711,
        amountSet: v724,
        approvalsCount: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
        sumOfSetAmounts: v711
        };
      await stdlib.mapSet(map1, v688, v725);
      const v726 = true;
      const v727 = await txn1.getOutput('CommunityMember_createClaim', 'v726', ctc3, v726);
      if (v307) {
        stdlib.protect(ctc0, await interact.out(v692, v727), {
          at: './index.rsh:144:15:application',
          fs: ['at ./index.rsh:144:15:application call to [unknown function] (defined at: ./index.rsh:144:15:function exp)', 'at ./index.rsh:170:29:application call to "sendResponse" (defined at: ./index.rsh:147:13:function exp)', 'at ./index.rsh:147:13:application call to [unknown function] (defined at: ./index.rsh:147:13:function exp)'],
          msg: 'out',
          who: 'CommunityMember_createClaim'
          });
        }
      else {
        }
      
      return;
      
      break;
      }
    case 'CommunityMember_payMonthlyFee0_53': {
      const v870 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_registerMembership0_53': {
      const v1048 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_respondToClaim0_53': {
      const v1226 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_stopContract0_53': {
      const v1404 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_withDrawClaim0_53': {
      const v1582 = v689[1];
      return;
      break;
      }
    }
  
  
  };
export async function _CommunityMember_payMonthlyFee4(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for _CommunityMember_payMonthlyFee4 expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for _CommunityMember_payMonthlyFee4 expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Null;
  const ctc1 = stdlib.T_Data({
    None: ctc0,
    Some: ctc0
    });
  const ctc2 = stdlib.T_UInt;
  const ctc3 = stdlib.T_Bool;
  const ctc4 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2]]);
  const ctc5 = stdlib.T_Data({
    None: ctc0,
    Some: ctc4
    });
  const ctc6 = stdlib.T_Struct([['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2]]);
  const ctc7 = stdlib.T_Data({
    None: ctc0,
    Some: ctc6
    });
  const ctc8 = stdlib.T_Address;
  const ctc9 = stdlib.T_Struct([['who', ctc8], ['mfee', ctc2]]);
  const ctc10 = stdlib.T_Tuple([ctc9]);
  const ctc11 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '200'));
  const ctc12 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2], ['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2], ['fundLimit', ctc2], ['description', ctc11]]);
  const ctc13 = stdlib.T_Tuple([ctc12]);
  const ctc14 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '60'));
  const ctc15 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '20'));
  const ctc16 = stdlib.T_Struct([['fullName', ctc14], ['phone', ctc15], ['email', ctc14], ['chosenInsurancePackage', ctc2]]);
  const ctc17 = stdlib.T_Tuple([ctc16]);
  const ctc18 = stdlib.T_Struct([['claimant', ctc8], ['accepted', ctc3], ['setAmount', ctc2]]);
  const ctc19 = stdlib.T_Tuple([ctc18]);
  const ctc20 = stdlib.T_Tuple([]);
  const ctc21 = stdlib.T_Data({
    CommunityMember_createClaim0_53: ctc13,
    CommunityMember_payMonthlyFee0_53: ctc10,
    CommunityMember_registerMembership0_53: ctc17,
    CommunityMember_respondToClaim0_53: ctc19,
    CommunityMember_stopContract0_53: ctc20,
    CommunityMember_withDrawClaim0_53: ctc20
    });
  
  const map0_ctc = ctc1;
  const map0 = stdlib.newMap({
    ctc: ctc,
    idx: 0,
    isAPI: true,
    ty: map0_ctc
    });
  
  const map1_ctc = ctc5;
  const map1 = stdlib.newMap({
    ctc: ctc,
    idx: 1,
    isAPI: true,
    ty: map1_ctc
    });
  
  const map2_ctc = ctc7;
  const map2 = stdlib.newMap({
    ctc: ctc,
    idx: 2,
    isAPI: true,
    ty: map2_ctc
    });
  
  
  const [v558, v559, v560, v578] = await ctc.getState(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '4'), [ctc8, ctc2, ctc3, ctc2]);
  const v600 = stdlib.protect(ctc10, await interact.in(), {
    at: './index.rsh:1:23:application',
    fs: ['at ./index.rsh:135:18:application call to [unknown function] (defined at: ./index.rsh:135:18:function exp)', 'at ./index.rsh:95:23:application call to "runCommunityMember_payMonthlyFee0_53" (defined at: ./index.rsh:134:14:function exp)', 'at ./index.rsh:95:23:application call to [unknown function] (defined at: ./index.rsh:95:23:function exp)'],
    msg: 'in',
    who: 'CommunityMember_payMonthlyFee'
    });
  const v601 = v600[stdlib.checkedBigNumberify('./index.rsh:1:23:application', stdlib.UInt_max, '0')];
  const v603 = v601.mfee;
  const v611 = ['CommunityMember_payMonthlyFee0_53', v600];
  
  const txn1 = await (ctc.sendrecv({
    args: [v558, v559, v560, v578, v611],
    evt_cnt: 1,
    funcNum: 3,
    lct: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc21],
    pay: [v603, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      stdlib.simMapDupe(sim_r, 0, map0);
      stdlib.simMapDupe(sim_r, 1, map1);
      stdlib.simMapDupe(sim_r, 2, map2);
      
      const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
      
      switch (v689[0]) {
        case 'CommunityMember_createClaim0_53': {
          const v692 = v689[1];
          
          break;
          }
        case 'CommunityMember_payMonthlyFee0_53': {
          const v870 = v689[1];
          sim_r.txns.push({
            kind: 'api',
            who: "CommunityMember_payMonthlyFee"
            });
          const v877 = v870[stdlib.checkedBigNumberify('./index.rsh:134:14:spread', stdlib.UInt_max, '0')];
          const v878 = v877.mfee;
          const v886 = stdlib.add(v578, v878);
          sim_r.txns.push({
            amt: v878,
            kind: 'to',
            tok: undefined /* Nothing */
            });
          const v934 = true;
          const v935 = await txn1.getOutput('CommunityMember_payMonthlyFee', 'v934', ctc3, v934);
          
          const v947 = stdlib.sub(v886, v878);
          sim_r.txns.push({
            kind: 'from',
            to: v558,
            tok: undefined /* Nothing */
            });
          const v3201 = v947;
          if (v560) {
            sim_r.isHalt = false;
            }
          else {
            sim_r.txns.push({
              kind: 'from',
              to: v558,
              tok: undefined /* Nothing */
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined /* Nothing */
              })
            sim_r.isHalt = true;
            }
          break;
          }
        case 'CommunityMember_registerMembership0_53': {
          const v1048 = v689[1];
          
          break;
          }
        case 'CommunityMember_respondToClaim0_53': {
          const v1226 = v689[1];
          
          break;
          }
        case 'CommunityMember_stopContract0_53': {
          const v1404 = v689[1];
          
          break;
          }
        case 'CommunityMember_withDrawClaim0_53': {
          const v1582 = v689[1];
          
          break;
          }
        }
      return sim_r;
      }),
    soloSend: false,
    timeoutAt: undefined /* mto */,
    tys: [ctc8, ctc2, ctc3, ctc2, ctc21],
    waitIfNotPresent: false
    }));
  const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
  switch (v689[0]) {
    case 'CommunityMember_createClaim0_53': {
      const v692 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_payMonthlyFee0_53': {
      const v870 = v689[1];
      undefined /* setApiDetails */;
      const v877 = v870[stdlib.checkedBigNumberify('./index.rsh:134:14:spread', stdlib.UInt_max, '0')];
      const v878 = v877.mfee;
      const v886 = stdlib.add(v578, v878);
      ;
      const v934 = true;
      const v935 = await txn1.getOutput('CommunityMember_payMonthlyFee', 'v934', ctc3, v934);
      if (v307) {
        stdlib.protect(ctc0, await interact.out(v870, v935), {
          at: './index.rsh:134:15:application',
          fs: ['at ./index.rsh:134:15:application call to [unknown function] (defined at: ./index.rsh:134:15:function exp)', 'at ./index.rsh:139:29:application call to "sendResponse" (defined at: ./index.rsh:137:13:function exp)', 'at ./index.rsh:137:13:application call to [unknown function] (defined at: ./index.rsh:137:13:function exp)'],
          msg: 'out',
          who: 'CommunityMember_payMonthlyFee'
          });
        }
      else {
        }
      
      const v947 = stdlib.sub(v886, v878);
      ;
      const v3201 = v947;
      if (v560) {
        return;
        }
      else {
        ;
        return;
        }
      break;
      }
    case 'CommunityMember_registerMembership0_53': {
      const v1048 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_respondToClaim0_53': {
      const v1226 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_stopContract0_53': {
      const v1404 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_withDrawClaim0_53': {
      const v1582 = v689[1];
      return;
      break;
      }
    }
  
  
  };
export async function _CommunityMember_registerMembership4(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for _CommunityMember_registerMembership4 expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for _CommunityMember_registerMembership4 expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Null;
  const ctc1 = stdlib.T_Data({
    None: ctc0,
    Some: ctc0
    });
  const ctc2 = stdlib.T_UInt;
  const ctc3 = stdlib.T_Bool;
  const ctc4 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2]]);
  const ctc5 = stdlib.T_Data({
    None: ctc0,
    Some: ctc4
    });
  const ctc6 = stdlib.T_Struct([['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2]]);
  const ctc7 = stdlib.T_Data({
    None: ctc0,
    Some: ctc6
    });
  const ctc8 = stdlib.T_Address;
  const ctc9 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '60'));
  const ctc10 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '20'));
  const ctc11 = stdlib.T_Struct([['fullName', ctc9], ['phone', ctc10], ['email', ctc9], ['chosenInsurancePackage', ctc2]]);
  const ctc12 = stdlib.T_Tuple([ctc11]);
  const ctc13 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '200'));
  const ctc14 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2], ['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2], ['fundLimit', ctc2], ['description', ctc13]]);
  const ctc15 = stdlib.T_Tuple([ctc14]);
  const ctc16 = stdlib.T_Struct([['who', ctc8], ['mfee', ctc2]]);
  const ctc17 = stdlib.T_Tuple([ctc16]);
  const ctc18 = stdlib.T_Struct([['claimant', ctc8], ['accepted', ctc3], ['setAmount', ctc2]]);
  const ctc19 = stdlib.T_Tuple([ctc18]);
  const ctc20 = stdlib.T_Tuple([]);
  const ctc21 = stdlib.T_Data({
    CommunityMember_createClaim0_53: ctc15,
    CommunityMember_payMonthlyFee0_53: ctc17,
    CommunityMember_registerMembership0_53: ctc12,
    CommunityMember_respondToClaim0_53: ctc19,
    CommunityMember_stopContract0_53: ctc20,
    CommunityMember_withDrawClaim0_53: ctc20
    });
  
  const map0_ctc = ctc1;
  const map0 = stdlib.newMap({
    ctc: ctc,
    idx: 0,
    isAPI: true,
    ty: map0_ctc
    });
  
  const map1_ctc = ctc5;
  const map1 = stdlib.newMap({
    ctc: ctc,
    idx: 1,
    isAPI: true,
    ty: map1_ctc
    });
  
  const map2_ctc = ctc7;
  const map2 = stdlib.newMap({
    ctc: ctc,
    idx: 2,
    isAPI: true,
    ty: map2_ctc
    });
  
  
  const [v558, v559, v560, v578] = await ctc.getState(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '4'), [ctc8, ctc2, ctc3, ctc2]);
  const v581 = stdlib.protect(ctc12, await interact.in(), {
    at: './index.rsh:1:23:application',
    fs: ['at ./index.rsh:118:17:application call to [unknown function] (defined at: ./index.rsh:118:17:function exp)', 'at ./index.rsh:95:23:application call to "runCommunityMember_registerMembership0_53" (defined at: ./index.rsh:117:13:function exp)', 'at ./index.rsh:95:23:application call to [unknown function] (defined at: ./index.rsh:95:23:function exp)'],
    msg: 'in',
    who: 'CommunityMember_registerMembership'
    });
  const v596 = ['CommunityMember_registerMembership0_53', v581];
  
  const txn1 = await (ctc.sendrecv({
    args: [v558, v559, v560, v578, v596],
    evt_cnt: 1,
    funcNum: 3,
    lct: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc21],
    pay: [v559, []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      stdlib.simMapDupe(sim_r, 0, map0);
      stdlib.simMapDupe(sim_r, 1, map1);
      stdlib.simMapDupe(sim_r, 2, map2);
      
      const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
      
      switch (v689[0]) {
        case 'CommunityMember_createClaim0_53': {
          const v692 = v689[1];
          
          break;
          }
        case 'CommunityMember_payMonthlyFee0_53': {
          const v870 = v689[1];
          
          break;
          }
        case 'CommunityMember_registerMembership0_53': {
          const v1048 = v689[1];
          sim_r.txns.push({
            kind: 'api',
            who: "CommunityMember_registerMembership"
            });
          const v1064 = stdlib.add(v578, v559);
          sim_r.txns.push({
            amt: v559,
            kind: 'to',
            tok: undefined /* Nothing */
            });
          const v1129 = true;
          const v1130 = await txn1.getOutput('CommunityMember_registerMembership', 'v1129', ctc3, v1129);
          
          const v1154 = stdlib.sub(v1064, v559);
          sim_r.txns.push({
            kind: 'from',
            to: v558,
            tok: undefined /* Nothing */
            });
          await stdlib.simMapSet(sim_r, 0, v688, null);
          const v3217 = v1154;
          if (v560) {
            sim_r.isHalt = false;
            }
          else {
            sim_r.txns.push({
              kind: 'from',
              to: v558,
              tok: undefined /* Nothing */
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined /* Nothing */
              })
            sim_r.isHalt = true;
            }
          break;
          }
        case 'CommunityMember_respondToClaim0_53': {
          const v1226 = v689[1];
          
          break;
          }
        case 'CommunityMember_stopContract0_53': {
          const v1404 = v689[1];
          
          break;
          }
        case 'CommunityMember_withDrawClaim0_53': {
          const v1582 = v689[1];
          
          break;
          }
        }
      return sim_r;
      }),
    soloSend: false,
    timeoutAt: undefined /* mto */,
    tys: [ctc8, ctc2, ctc3, ctc2, ctc21],
    waitIfNotPresent: false
    }));
  const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
  switch (v689[0]) {
    case 'CommunityMember_createClaim0_53': {
      const v692 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_payMonthlyFee0_53': {
      const v870 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_registerMembership0_53': {
      const v1048 = v689[1];
      undefined /* setApiDetails */;
      const v1064 = stdlib.add(v578, v559);
      ;
      const v1129 = true;
      const v1130 = await txn1.getOutput('CommunityMember_registerMembership', 'v1129', ctc3, v1129);
      if (v307) {
        stdlib.protect(ctc0, await interact.out(v1048, v1130), {
          at: './index.rsh:117:14:application',
          fs: ['at ./index.rsh:117:14:application call to [unknown function] (defined at: ./index.rsh:117:14:function exp)', 'at ./index.rsh:122:29:application call to "sendResponse" (defined at: ./index.rsh:120:13:function exp)', 'at ./index.rsh:120:13:application call to [unknown function] (defined at: ./index.rsh:120:13:function exp)'],
          msg: 'out',
          who: 'CommunityMember_registerMembership'
          });
        }
      else {
        }
      
      const v1154 = stdlib.sub(v1064, v559);
      ;
      await stdlib.mapSet(map0, v688, null);
      const v3217 = v1154;
      if (v560) {
        return;
        }
      else {
        ;
        return;
        }
      break;
      }
    case 'CommunityMember_respondToClaim0_53': {
      const v1226 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_stopContract0_53': {
      const v1404 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_withDrawClaim0_53': {
      const v1582 = v689[1];
      return;
      break;
      }
    }
  
  
  };
export async function _CommunityMember_respondToClaim4(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for _CommunityMember_respondToClaim4 expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for _CommunityMember_respondToClaim4 expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Null;
  const ctc1 = stdlib.T_Data({
    None: ctc0,
    Some: ctc0
    });
  const ctc2 = stdlib.T_UInt;
  const ctc3 = stdlib.T_Bool;
  const ctc4 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2]]);
  const ctc5 = stdlib.T_Data({
    None: ctc0,
    Some: ctc4
    });
  const ctc6 = stdlib.T_Struct([['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2]]);
  const ctc7 = stdlib.T_Data({
    None: ctc0,
    Some: ctc6
    });
  const ctc8 = stdlib.T_Address;
  const ctc9 = stdlib.T_Struct([['claimant', ctc8], ['accepted', ctc3], ['setAmount', ctc2]]);
  const ctc10 = stdlib.T_Tuple([ctc9]);
  const ctc11 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '200'));
  const ctc12 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2], ['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2], ['fundLimit', ctc2], ['description', ctc11]]);
  const ctc13 = stdlib.T_Tuple([ctc12]);
  const ctc14 = stdlib.T_Struct([['who', ctc8], ['mfee', ctc2]]);
  const ctc15 = stdlib.T_Tuple([ctc14]);
  const ctc16 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '60'));
  const ctc17 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '20'));
  const ctc18 = stdlib.T_Struct([['fullName', ctc16], ['phone', ctc17], ['email', ctc16], ['chosenInsurancePackage', ctc2]]);
  const ctc19 = stdlib.T_Tuple([ctc18]);
  const ctc20 = stdlib.T_Tuple([]);
  const ctc21 = stdlib.T_Data({
    CommunityMember_createClaim0_53: ctc13,
    CommunityMember_payMonthlyFee0_53: ctc15,
    CommunityMember_registerMembership0_53: ctc19,
    CommunityMember_respondToClaim0_53: ctc10,
    CommunityMember_stopContract0_53: ctc20,
    CommunityMember_withDrawClaim0_53: ctc20
    });
  
  const map0_ctc = ctc1;
  const map0 = stdlib.newMap({
    ctc: ctc,
    idx: 0,
    isAPI: true,
    ty: map0_ctc
    });
  
  const map1_ctc = ctc5;
  const map1 = stdlib.newMap({
    ctc: ctc,
    idx: 1,
    isAPI: true,
    ty: map1_ctc
    });
  
  const map2_ctc = ctc7;
  const map2 = stdlib.newMap({
    ctc: ctc,
    idx: 2,
    isAPI: true,
    ty: map2_ctc
    });
  
  
  const [v558, v559, v560, v578] = await ctc.getState(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '4'), [ctc8, ctc2, ctc3, ctc2]);
  const v646 = stdlib.protect(ctc10, await interact.in(), {
    at: './index.rsh:1:23:application',
    fs: ['at ./index.rsh:180:17:application call to [unknown function] (defined at: ./index.rsh:180:17:function exp)', 'at ./index.rsh:95:23:application call to "runCommunityMember_respondToClaim0_53" (defined at: ./index.rsh:179:14:function exp)', 'at ./index.rsh:95:23:application call to [unknown function] (defined at: ./index.rsh:95:23:function exp)'],
    msg: 'in',
    who: 'CommunityMember_respondToClaim'
    });
  const v659 = ['CommunityMember_respondToClaim0_53', v646];
  
  const txn1 = await (ctc.sendrecv({
    args: [v558, v559, v560, v578, v659],
    evt_cnt: 1,
    funcNum: 3,
    lct: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc21],
    pay: [stdlib.checkedBigNumberify('./index.rsh:181:20:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      stdlib.simMapDupe(sim_r, 0, map0);
      stdlib.simMapDupe(sim_r, 1, map1);
      stdlib.simMapDupe(sim_r, 2, map2);
      
      const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
      
      switch (v689[0]) {
        case 'CommunityMember_createClaim0_53': {
          const v692 = v689[1];
          
          break;
          }
        case 'CommunityMember_payMonthlyFee0_53': {
          const v870 = v689[1];
          
          break;
          }
        case 'CommunityMember_registerMembership0_53': {
          const v1048 = v689[1];
          
          break;
          }
        case 'CommunityMember_respondToClaim0_53': {
          const v1226 = v689[1];
          sim_r.txns.push({
            kind: 'api',
            who: "CommunityMember_respondToClaim"
            });
          ;
          const v1337 = v1226[stdlib.checkedBigNumberify('./index.rsh:179:14:spread', stdlib.UInt_max, '0')];
          const v1338 = v1337.claimant;
          const v1339 = true;
          const v1340 = await txn1.getOutput('CommunityMember_respondToClaim', 'v1339', ctc3, v1339);
          
          const v1351 = v1337.accepted;
          if (v1351) {
            const v1352 = stdlib.protect(map1_ctc, await stdlib.simMapRef(sim_r, 1, v1338), null);
            let v1353;
            switch (v1352[0]) {
              case 'None': {
                const v1354 = v1352[1];
                v1353 = stdlib.checkedBigNumberify('./index.rsh:188:73:decimal', stdlib.UInt_max, '1');
                
                break;
                }
              case 'Some': {
                const v1355 = v1352[1];
                const v1356 = v1355.approvalsCount;
                v1353 = v1356;
                
                break;
                }
              }
            let v1358;
            switch (v1352[0]) {
              case 'None': {
                const v1359 = v1352[1];
                v1358 = stdlib.checkedBigNumberify('./index.rsh:189:73:decimal', stdlib.UInt_max, '0');
                
                break;
                }
              case 'Some': {
                const v1360 = v1352[1];
                const v1361 = v1360.sumOfSetAmounts;
                v1358 = v1361;
                
                break;
                }
              }
            let v1363;
            switch (v1352[0]) {
              case 'None': {
                const v1364 = v1352[1];
                v1363 = stdlib.checkedBigNumberify('./index.rsh:190:70:decimal', stdlib.UInt_max, '0');
                
                break;
                }
              case 'Some': {
                const v1365 = v1352[1];
                const v1366 = v1365.amountRequested;
                v1363 = v1366;
                
                break;
                }
              }
            let v1368;
            switch (v1352[0]) {
              case 'None': {
                const v1369 = v1352[1];
                v1368 = v1363;
                
                break;
                }
              case 'Some': {
                const v1370 = v1352[1];
                const v1371 = v1370.amountSet;
                v1368 = v1371;
                
                break;
                }
              }
            const v1372 = stdlib.lt(v1353, stdlib.checkedBigNumberify('./index.rsh:192:63:decimal', stdlib.UInt_max, '5'));
            const v1373 = stdlib.div(v1358, v1353);
            const v1374 = v1372 ? v1368 : v1373;
            const v1375 = stdlib.add(v1353, stdlib.checkedBigNumberify('./index.rsh:194:56:decimal', stdlib.UInt_max, '1'));
            const v1376 = {
              accepted: true,
              amountRequested: v1363,
              amountSet: v1374,
              approvalsCount: v1375,
              sumOfSetAmounts: v1358
              };
            await stdlib.simMapSet(sim_r, 1, v1338, v1376);
            const v1377 = stdlib.ge(v1353, stdlib.checkedBigNumberify('./index.rsh:201:41:decimal', stdlib.UInt_max, '5'));
            if (v1377) {
              await stdlib.simMapSet(sim_r, 2, v1338, undefined /* Nothing */);
              await stdlib.simMapSet(sim_r, 1, v1338, undefined /* Nothing */);
              const v3233 = v578;
              if (v560) {
                sim_r.isHalt = false;
                }
              else {
                sim_r.txns.push({
                  kind: 'from',
                  to: v558,
                  tok: undefined /* Nothing */
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined /* Nothing */
                  })
                sim_r.isHalt = true;
                }}
            else {
              const v3235 = v578;
              if (v560) {
                sim_r.isHalt = false;
                }
              else {
                sim_r.txns.push({
                  kind: 'from',
                  to: v558,
                  tok: undefined /* Nothing */
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined /* Nothing */
                  })
                sim_r.isHalt = true;
                }}}
          else {
            const v3237 = v578;
            if (v560) {
              sim_r.isHalt = false;
              }
            else {
              sim_r.txns.push({
                kind: 'from',
                to: v558,
                tok: undefined /* Nothing */
                });
              sim_r.txns.push({
                kind: 'halt',
                tok: undefined /* Nothing */
                })
              sim_r.isHalt = true;
              }}
          break;
          }
        case 'CommunityMember_stopContract0_53': {
          const v1404 = v689[1];
          
          break;
          }
        case 'CommunityMember_withDrawClaim0_53': {
          const v1582 = v689[1];
          
          break;
          }
        }
      return sim_r;
      }),
    soloSend: false,
    timeoutAt: undefined /* mto */,
    tys: [ctc8, ctc2, ctc3, ctc2, ctc21],
    waitIfNotPresent: false
    }));
  const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
  switch (v689[0]) {
    case 'CommunityMember_createClaim0_53': {
      const v692 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_payMonthlyFee0_53': {
      const v870 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_registerMembership0_53': {
      const v1048 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_respondToClaim0_53': {
      const v1226 = v689[1];
      undefined /* setApiDetails */;
      ;
      const v1337 = v1226[stdlib.checkedBigNumberify('./index.rsh:179:14:spread', stdlib.UInt_max, '0')];
      const v1338 = v1337.claimant;
      const v1339 = true;
      const v1340 = await txn1.getOutput('CommunityMember_respondToClaim', 'v1339', ctc3, v1339);
      if (v307) {
        stdlib.protect(ctc0, await interact.out(v1226, v1340), {
          at: './index.rsh:179:15:application',
          fs: ['at ./index.rsh:179:15:application call to [unknown function] (defined at: ./index.rsh:179:15:function exp)', 'at ./index.rsh:185:29:application call to "sendResponse" (defined at: ./index.rsh:182:13:function exp)', 'at ./index.rsh:182:13:application call to [unknown function] (defined at: ./index.rsh:182:13:function exp)'],
          msg: 'out',
          who: 'CommunityMember_respondToClaim'
          });
        }
      else {
        }
      
      const v1351 = v1337.accepted;
      if (v1351) {
        const v1352 = stdlib.protect(map1_ctc, await stdlib.mapRef(map1, v1338), null);
        let v1353;
        switch (v1352[0]) {
          case 'None': {
            const v1354 = v1352[1];
            v1353 = stdlib.checkedBigNumberify('./index.rsh:188:73:decimal', stdlib.UInt_max, '1');
            
            break;
            }
          case 'Some': {
            const v1355 = v1352[1];
            const v1356 = v1355.approvalsCount;
            v1353 = v1356;
            
            break;
            }
          }
        let v1358;
        switch (v1352[0]) {
          case 'None': {
            const v1359 = v1352[1];
            v1358 = stdlib.checkedBigNumberify('./index.rsh:189:73:decimal', stdlib.UInt_max, '0');
            
            break;
            }
          case 'Some': {
            const v1360 = v1352[1];
            const v1361 = v1360.sumOfSetAmounts;
            v1358 = v1361;
            
            break;
            }
          }
        let v1363;
        switch (v1352[0]) {
          case 'None': {
            const v1364 = v1352[1];
            v1363 = stdlib.checkedBigNumberify('./index.rsh:190:70:decimal', stdlib.UInt_max, '0');
            
            break;
            }
          case 'Some': {
            const v1365 = v1352[1];
            const v1366 = v1365.amountRequested;
            v1363 = v1366;
            
            break;
            }
          }
        let v1368;
        switch (v1352[0]) {
          case 'None': {
            const v1369 = v1352[1];
            v1368 = v1363;
            
            break;
            }
          case 'Some': {
            const v1370 = v1352[1];
            const v1371 = v1370.amountSet;
            v1368 = v1371;
            
            break;
            }
          }
        const v1372 = stdlib.lt(v1353, stdlib.checkedBigNumberify('./index.rsh:192:63:decimal', stdlib.UInt_max, '5'));
        const v1373 = stdlib.div(v1358, v1353);
        const v1374 = v1372 ? v1368 : v1373;
        const v1375 = stdlib.add(v1353, stdlib.checkedBigNumberify('./index.rsh:194:56:decimal', stdlib.UInt_max, '1'));
        const v1376 = {
          accepted: true,
          amountRequested: v1363,
          amountSet: v1374,
          approvalsCount: v1375,
          sumOfSetAmounts: v1358
          };
        await stdlib.mapSet(map1, v1338, v1376);
        const v1377 = stdlib.ge(v1353, stdlib.checkedBigNumberify('./index.rsh:201:41:decimal', stdlib.UInt_max, '5'));
        if (v1377) {
          await stdlib.mapSet(map2, v1338, undefined /* Nothing */);
          await stdlib.mapSet(map1, v1338, undefined /* Nothing */);
          const v3233 = v578;
          if (v560) {
            return;
            }
          else {
            ;
            return;
            }}
        else {
          const v3235 = v578;
          if (v560) {
            return;
            }
          else {
            ;
            return;
            }}}
      else {
        const v3237 = v578;
        if (v560) {
          return;
          }
        else {
          ;
          return;
          }}
      break;
      }
    case 'CommunityMember_stopContract0_53': {
      const v1404 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_withDrawClaim0_53': {
      const v1582 = v689[1];
      return;
      break;
      }
    }
  
  
  };
export async function _CommunityMember_stopContract4(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for _CommunityMember_stopContract4 expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for _CommunityMember_stopContract4 expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Null;
  const ctc1 = stdlib.T_Data({
    None: ctc0,
    Some: ctc0
    });
  const ctc2 = stdlib.T_UInt;
  const ctc3 = stdlib.T_Bool;
  const ctc4 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2]]);
  const ctc5 = stdlib.T_Data({
    None: ctc0,
    Some: ctc4
    });
  const ctc6 = stdlib.T_Struct([['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2]]);
  const ctc7 = stdlib.T_Data({
    None: ctc0,
    Some: ctc6
    });
  const ctc8 = stdlib.T_Address;
  const ctc9 = stdlib.T_Tuple([]);
  const ctc10 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '200'));
  const ctc11 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2], ['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2], ['fundLimit', ctc2], ['description', ctc10]]);
  const ctc12 = stdlib.T_Tuple([ctc11]);
  const ctc13 = stdlib.T_Struct([['who', ctc8], ['mfee', ctc2]]);
  const ctc14 = stdlib.T_Tuple([ctc13]);
  const ctc15 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '60'));
  const ctc16 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '20'));
  const ctc17 = stdlib.T_Struct([['fullName', ctc15], ['phone', ctc16], ['email', ctc15], ['chosenInsurancePackage', ctc2]]);
  const ctc18 = stdlib.T_Tuple([ctc17]);
  const ctc19 = stdlib.T_Struct([['claimant', ctc8], ['accepted', ctc3], ['setAmount', ctc2]]);
  const ctc20 = stdlib.T_Tuple([ctc19]);
  const ctc21 = stdlib.T_Data({
    CommunityMember_createClaim0_53: ctc12,
    CommunityMember_payMonthlyFee0_53: ctc14,
    CommunityMember_registerMembership0_53: ctc18,
    CommunityMember_respondToClaim0_53: ctc20,
    CommunityMember_stopContract0_53: ctc9,
    CommunityMember_withDrawClaim0_53: ctc9
    });
  
  const map0_ctc = ctc1;
  const map0 = stdlib.newMap({
    ctc: ctc,
    idx: 0,
    isAPI: true,
    ty: map0_ctc
    });
  
  const map1_ctc = ctc5;
  const map1 = stdlib.newMap({
    ctc: ctc,
    idx: 1,
    isAPI: true,
    ty: map1_ctc
    });
  
  const map2_ctc = ctc7;
  const map2 = stdlib.newMap({
    ctc: ctc,
    idx: 2,
    isAPI: true,
    ty: map2_ctc
    });
  
  
  const [v558, v559, v560, v578] = await ctc.getState(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '4'), [ctc8, ctc2, ctc3, ctc2]);
  const v671 = stdlib.protect(ctc9, await interact.in(), {
    at: './index.rsh:1:23:application',
    fs: ['at ./index.rsh:238:16:application call to [unknown function] (defined at: ./index.rsh:238:16:function exp)', 'at ./index.rsh:95:23:application call to "runCommunityMember_stopContract0_53" (defined at: ./index.rsh:237:14:function exp)', 'at ./index.rsh:95:23:application call to [unknown function] (defined at: ./index.rsh:95:23:function exp)'],
    msg: 'in',
    who: 'CommunityMember_stopContract'
    });
  const v675 = ['CommunityMember_stopContract0_53', v671];
  
  const txn1 = await (ctc.sendrecv({
    args: [v558, v559, v560, v578, v675],
    evt_cnt: 1,
    funcNum: 3,
    lct: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc21],
    pay: [stdlib.checkedBigNumberify('./index.rsh:239:19:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      stdlib.simMapDupe(sim_r, 0, map0);
      stdlib.simMapDupe(sim_r, 1, map1);
      stdlib.simMapDupe(sim_r, 2, map2);
      
      const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
      
      switch (v689[0]) {
        case 'CommunityMember_createClaim0_53': {
          const v692 = v689[1];
          
          break;
          }
        case 'CommunityMember_payMonthlyFee0_53': {
          const v870 = v689[1];
          
          break;
          }
        case 'CommunityMember_registerMembership0_53': {
          const v1048 = v689[1];
          
          break;
          }
        case 'CommunityMember_respondToClaim0_53': {
          const v1226 = v689[1];
          
          break;
          }
        case 'CommunityMember_stopContract0_53': {
          const v1404 = v689[1];
          sim_r.txns.push({
            kind: 'api',
            who: "CommunityMember_stopContract"
            });
          ;
          const v1562 = true;
          const v1563 = await txn1.getOutput('CommunityMember_stopContract', 'v1562', ctc3, v1562);
          
          const v3253 = v578;
          if (v560) {
            sim_r.isHalt = false;
            }
          else {
            sim_r.txns.push({
              kind: 'from',
              to: v558,
              tok: undefined /* Nothing */
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined /* Nothing */
              })
            sim_r.isHalt = true;
            }
          break;
          }
        case 'CommunityMember_withDrawClaim0_53': {
          const v1582 = v689[1];
          
          break;
          }
        }
      return sim_r;
      }),
    soloSend: false,
    timeoutAt: undefined /* mto */,
    tys: [ctc8, ctc2, ctc3, ctc2, ctc21],
    waitIfNotPresent: false
    }));
  const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
  switch (v689[0]) {
    case 'CommunityMember_createClaim0_53': {
      const v692 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_payMonthlyFee0_53': {
      const v870 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_registerMembership0_53': {
      const v1048 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_respondToClaim0_53': {
      const v1226 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_stopContract0_53': {
      const v1404 = v689[1];
      undefined /* setApiDetails */;
      ;
      const v1562 = true;
      const v1563 = await txn1.getOutput('CommunityMember_stopContract', 'v1562', ctc3, v1562);
      if (v307) {
        stdlib.protect(ctc0, await interact.out(v1404, v1563), {
          at: './index.rsh:237:15:application',
          fs: ['at ./index.rsh:237:15:application call to [unknown function] (defined at: ./index.rsh:237:15:function exp)', 'at ./index.rsh:246:29:application call to "sendResponse" (defined at: ./index.rsh:240:13:function exp)', 'at ./index.rsh:240:13:application call to [unknown function] (defined at: ./index.rsh:240:13:function exp)'],
          msg: 'out',
          who: 'CommunityMember_stopContract'
          });
        }
      else {
        }
      
      const v3253 = v578;
      if (v560) {
        return;
        }
      else {
        ;
        return;
        }
      break;
      }
    case 'CommunityMember_withDrawClaim0_53': {
      const v1582 = v689[1];
      return;
      break;
      }
    }
  
  
  };
export async function _CommunityMember_withDrawClaim4(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for _CommunityMember_withDrawClaim4 expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for _CommunityMember_withDrawClaim4 expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Null;
  const ctc1 = stdlib.T_Data({
    None: ctc0,
    Some: ctc0
    });
  const ctc2 = stdlib.T_UInt;
  const ctc3 = stdlib.T_Bool;
  const ctc4 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2]]);
  const ctc5 = stdlib.T_Data({
    None: ctc0,
    Some: ctc4
    });
  const ctc6 = stdlib.T_Struct([['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2]]);
  const ctc7 = stdlib.T_Data({
    None: ctc0,
    Some: ctc6
    });
  const ctc8 = stdlib.T_Address;
  const ctc9 = stdlib.T_Tuple([]);
  const ctc10 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '200'));
  const ctc11 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2], ['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2], ['fundLimit', ctc2], ['description', ctc10]]);
  const ctc12 = stdlib.T_Tuple([ctc11]);
  const ctc13 = stdlib.T_Struct([['who', ctc8], ['mfee', ctc2]]);
  const ctc14 = stdlib.T_Tuple([ctc13]);
  const ctc15 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '60'));
  const ctc16 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '20'));
  const ctc17 = stdlib.T_Struct([['fullName', ctc15], ['phone', ctc16], ['email', ctc15], ['chosenInsurancePackage', ctc2]]);
  const ctc18 = stdlib.T_Tuple([ctc17]);
  const ctc19 = stdlib.T_Struct([['claimant', ctc8], ['accepted', ctc3], ['setAmount', ctc2]]);
  const ctc20 = stdlib.T_Tuple([ctc19]);
  const ctc21 = stdlib.T_Data({
    CommunityMember_createClaim0_53: ctc12,
    CommunityMember_payMonthlyFee0_53: ctc14,
    CommunityMember_registerMembership0_53: ctc18,
    CommunityMember_respondToClaim0_53: ctc20,
    CommunityMember_stopContract0_53: ctc9,
    CommunityMember_withDrawClaim0_53: ctc9
    });
  
  const map0_ctc = ctc1;
  const map0 = stdlib.newMap({
    ctc: ctc,
    idx: 0,
    isAPI: true,
    ty: map0_ctc
    });
  
  const map1_ctc = ctc5;
  const map1 = stdlib.newMap({
    ctc: ctc,
    idx: 1,
    isAPI: true,
    ty: map1_ctc
    });
  
  const map2_ctc = ctc7;
  const map2 = stdlib.newMap({
    ctc: ctc,
    idx: 2,
    isAPI: true,
    ty: map2_ctc
    });
  
  
  const [v558, v559, v560, v578] = await ctc.getState(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '4'), [ctc8, ctc2, ctc3, ctc2]);
  const v663 = stdlib.protect(ctc9, await interact.in(), {
    at: './index.rsh:1:23:application',
    fs: ['at ./index.rsh:218:16:application call to [unknown function] (defined at: ./index.rsh:218:16:function exp)', 'at ./index.rsh:95:23:application call to "runCommunityMember_withDrawClaim0_53" (defined at: ./index.rsh:217:14:function exp)', 'at ./index.rsh:95:23:application call to [unknown function] (defined at: ./index.rsh:95:23:function exp)'],
    msg: 'in',
    who: 'CommunityMember_withDrawClaim'
    });
  const v667 = ['CommunityMember_withDrawClaim0_53', v663];
  
  const txn1 = await (ctc.sendrecv({
    args: [v558, v559, v560, v578, v667],
    evt_cnt: 1,
    funcNum: 3,
    lct: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc21],
    pay: [stdlib.checkedBigNumberify('./index.rsh:219:19:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      stdlib.simMapDupe(sim_r, 0, map0);
      stdlib.simMapDupe(sim_r, 1, map1);
      stdlib.simMapDupe(sim_r, 2, map2);
      
      const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
      
      switch (v689[0]) {
        case 'CommunityMember_createClaim0_53': {
          const v692 = v689[1];
          
          break;
          }
        case 'CommunityMember_payMonthlyFee0_53': {
          const v870 = v689[1];
          
          break;
          }
        case 'CommunityMember_registerMembership0_53': {
          const v1048 = v689[1];
          
          break;
          }
        case 'CommunityMember_respondToClaim0_53': {
          const v1226 = v689[1];
          
          break;
          }
        case 'CommunityMember_stopContract0_53': {
          const v1404 = v689[1];
          
          break;
          }
        case 'CommunityMember_withDrawClaim0_53': {
          const v1582 = v689[1];
          sim_r.txns.push({
            kind: 'api',
            who: "CommunityMember_withDrawClaim"
            });
          ;
          const v1748 = true;
          const v1749 = await txn1.getOutput('CommunityMember_withDrawClaim', 'v1748', ctc3, v1748);
          
          await stdlib.simMapSet(sim_r, 1, v688, undefined /* Nothing */);
          await stdlib.simMapSet(sim_r, 2, v688, undefined /* Nothing */);
          const v3269 = v578;
          if (v560) {
            sim_r.isHalt = false;
            }
          else {
            sim_r.txns.push({
              kind: 'from',
              to: v558,
              tok: undefined /* Nothing */
              });
            sim_r.txns.push({
              kind: 'halt',
              tok: undefined /* Nothing */
              })
            sim_r.isHalt = true;
            }
          break;
          }
        }
      return sim_r;
      }),
    soloSend: false,
    timeoutAt: undefined /* mto */,
    tys: [ctc8, ctc2, ctc3, ctc2, ctc21],
    waitIfNotPresent: false
    }));
  const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn1;
  switch (v689[0]) {
    case 'CommunityMember_createClaim0_53': {
      const v692 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_payMonthlyFee0_53': {
      const v870 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_registerMembership0_53': {
      const v1048 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_respondToClaim0_53': {
      const v1226 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_stopContract0_53': {
      const v1404 = v689[1];
      return;
      break;
      }
    case 'CommunityMember_withDrawClaim0_53': {
      const v1582 = v689[1];
      undefined /* setApiDetails */;
      ;
      const v1748 = true;
      const v1749 = await txn1.getOutput('CommunityMember_withDrawClaim', 'v1748', ctc3, v1748);
      if (v307) {
        stdlib.protect(ctc0, await interact.out(v1582, v1749), {
          at: './index.rsh:217:15:application',
          fs: ['at ./index.rsh:217:15:application call to [unknown function] (defined at: ./index.rsh:217:15:function exp)', 'at ./index.rsh:222:29:application call to "sendResponse" (defined at: ./index.rsh:220:28:function exp)', 'at ./index.rsh:220:28:application call to [unknown function] (defined at: ./index.rsh:220:28:function exp)'],
          msg: 'out',
          who: 'CommunityMember_withDrawClaim'
          });
        }
      else {
        }
      
      await stdlib.mapSet(map1, v688, undefined /* Nothing */);
      await stdlib.mapSet(map2, v688, undefined /* Nothing */);
      const v3269 = v578;
      if (v560) {
        return;
        }
      else {
        ;
        return;
        }
      break;
      }
    }
  
  
  };
export async function Insurer(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for Insurer expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Insurer expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Null;
  const ctc1 = stdlib.T_Data({
    None: ctc0,
    Some: ctc0
    });
  const ctc2 = stdlib.T_UInt;
  const ctc3 = stdlib.T_Bool;
  const ctc4 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2]]);
  const ctc5 = stdlib.T_Data({
    None: ctc0,
    Some: ctc4
    });
  const ctc6 = stdlib.T_Struct([['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2]]);
  const ctc7 = stdlib.T_Data({
    None: ctc0,
    Some: ctc6
    });
  const ctc8 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '200'));
  const ctc9 = stdlib.T_Struct([['amountRequested', ctc2], ['amountSet', ctc2], ['accepted', ctc3], ['approvalsCount', ctc2], ['sumOfSetAmounts', ctc2], ['insrPackageId', ctc2], ['amountDue', ctc2], ['matureBalance', ctc2], ['fundLimit', ctc2], ['description', ctc8]]);
  const ctc10 = stdlib.T_Tuple([ctc9]);
  const ctc11 = stdlib.T_Address;
  const ctc12 = stdlib.T_Struct([['who', ctc11], ['mfee', ctc2]]);
  const ctc13 = stdlib.T_Tuple([ctc12]);
  const ctc14 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '60'));
  const ctc15 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '20'));
  const ctc16 = stdlib.T_Struct([['fullName', ctc14], ['phone', ctc15], ['email', ctc14], ['chosenInsurancePackage', ctc2]]);
  const ctc17 = stdlib.T_Tuple([ctc16]);
  const ctc18 = stdlib.T_Struct([['claimant', ctc11], ['accepted', ctc3], ['setAmount', ctc2]]);
  const ctc19 = stdlib.T_Tuple([ctc18]);
  const ctc20 = stdlib.T_Tuple([]);
  const ctc21 = stdlib.T_Data({
    CommunityMember_createClaim0_53: ctc10,
    CommunityMember_payMonthlyFee0_53: ctc13,
    CommunityMember_registerMembership0_53: ctc17,
    CommunityMember_respondToClaim0_53: ctc19,
    CommunityMember_stopContract0_53: ctc20,
    CommunityMember_withDrawClaim0_53: ctc20
    });
  
  const map0_ctc = ctc1;
  const map0 = stdlib.newMap({
    ctc: ctc,
    idx: 0,
    isAPI: false,
    ty: map0_ctc
    });
  
  const map1_ctc = ctc5;
  const map1 = stdlib.newMap({
    ctc: ctc,
    idx: 1,
    isAPI: false,
    ty: map1_ctc
    });
  
  const map2_ctc = ctc7;
  const map2 = stdlib.newMap({
    ctc: ctc,
    idx: 2,
    isAPI: false,
    ty: map2_ctc
    });
  
  
  const v554 = stdlib.protect(ctc3, interact.contractIsRunning, 'for Insurer\'s interact field contractIsRunning');
  const v555 = stdlib.protect(ctc2, interact.mandatoryEntryFee, 'for Insurer\'s interact field mandatoryEntryFee');
  
  stdlib.protect(ctc0, await interact.seeFeedback(), {
    at: './index.rsh:62:29:application',
    fs: ['at ./index.rsh:59:17:application call to [unknown function] (defined at: ./index.rsh:59:21:function exp)'],
    msg: 'seeFeedback',
    who: 'Insurer'
    });
  
  const txn1 = await (ctc.sendrecv({
    args: [v555, v554],
    evt_cnt: 2,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./index.rsh:64:13:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc2, ctc3],
    pay: [stdlib.checkedBigNumberify('./index.rsh:64:13:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      stdlib.simMapDupe(sim_r, 0, map0);
      stdlib.simMapDupe(sim_r, 1, map1);
      stdlib.simMapDupe(sim_r, 2, map2);
      
      const {data: [v559, v560], secs: v562, time: v561, didSend: v30, from: v558 } = txn1;
      
      ;
      
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc2, ctc3],
    waitIfNotPresent: false
    }));
  const {data: [v559, v560], secs: v562, time: v561, didSend: v30, from: v558 } = txn1;
  ;
  const v564 = 'backend: starting...';
  stdlib.protect(ctc0, await interact.log(v564), {
    at: './index.rsh:66:25:application',
    fs: ['at ./index.rsh:66:25:application call to [unknown function] (defined at: ./index.rsh:66:25:function exp)', 'at ./index.rsh:66:25:application call to "liftedInteract" (defined at: ./index.rsh:66:25:application)'],
    msg: 'log',
    who: 'Insurer'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v558, v559, v560],
    evt_cnt: 0,
    funcNum: 1,
    lct: v561,
    onlyIf: true,
    out_tys: [],
    pay: [stdlib.checkedBigNumberify('./index.rsh:68:13:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      stdlib.simMapDupe(sim_r, 0, map0);
      stdlib.simMapDupe(sim_r, 1, map1);
      stdlib.simMapDupe(sim_r, 2, map2);
      
      const {data: [], secs: v567, time: v566, didSend: v40, from: v565 } = txn2;
      
      ;
      const v571 = v566;
      const v578 = stdlib.checkedBigNumberify('./index.rsh:54:11:after expr stmt semicolon', stdlib.UInt_max, '0');
      
      if (await (async () => {
        
        return v560;})()) {
        sim_r.isHalt = false;
        }
      else {
        sim_r.txns.push({
          kind: 'from',
          to: v558,
          tok: undefined /* Nothing */
          });
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined /* Nothing */
          })
        sim_r.isHalt = true;
        }
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc11, ctc2, ctc3],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v567, time: v566, didSend: v40, from: v565 } = txn2;
  ;
  const v568 = stdlib.addressEq(v558, v565);
  stdlib.assert(v568, {
    at: './index.rsh:68:13:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Insurer'
    });
  let v571 = v566;
  let v578 = stdlib.checkedBigNumberify('./index.rsh:54:11:after expr stmt semicolon', stdlib.UInt_max, '0');
  
  while (await (async () => {
    
    return v560;})()) {
    const txn3 = await (ctc.recv({
      didSend: false,
      evt_cnt: 1,
      funcNum: 3,
      out_tys: [ctc21],
      timeoutAt: undefined /* mto */,
      waitIfNotPresent: false
      }));
    const {data: [v689], secs: v691, time: v690, didSend: v307, from: v688 } = txn3;
    switch (v689[0]) {
      case 'CommunityMember_createClaim0_53': {
        const v692 = v689[1];
        undefined /* setApiDetails */;
        ;
        const v710 = v692[stdlib.checkedBigNumberify('./index.rsh:144:14:spread', stdlib.UInt_max, '0')];
        const v711 = v710.amountRequested;
        const v712 = v710.description;
        const v714 = {
          amountRequested: v711,
          description: v712
          };
        stdlib.protect(ctc0, await interact.saveNewClaim(v714), {
          at: './index.rsh:149:46:application',
          fs: ['at ./index.rsh:149:46:application call to [unknown function] (defined at: ./index.rsh:149:46:function exp)', 'at ./index.rsh:149:46:application call to "liftedInteract" (defined at: ./index.rsh:149:46:application)', 'at ./index.rsh:147:13:application call to [unknown function] (defined at: ./index.rsh:147:13:function exp)'],
          msg: 'saveNewClaim',
          who: 'Insurer'
          });
        
        const v716 = 'backend: API.CommunityMember.createClaim ...';
        stdlib.protect(ctc0, await interact.log(v716), {
          at: './index.rsh:151:37:application',
          fs: ['at ./index.rsh:151:37:application call to [unknown function] (defined at: ./index.rsh:151:37:function exp)', 'at ./index.rsh:151:37:application call to "liftedInteract" (defined at: ./index.rsh:151:37:application)', 'at ./index.rsh:147:13:application call to [unknown function] (defined at: ./index.rsh:147:13:function exp)'],
          msg: 'log',
          who: 'Insurer'
          });
        
        const v717 = v710.insrPackageId;
        const v718 = v710.amountDue;
        const v719 = v710.matureBalance;
        const v720 = {
          amountDue: v718,
          insrPackageId: v717,
          matureBalance: v719
          };
        await stdlib.mapSet(map2, v688, v720);
        const v721 = v710.fundLimit;
        const v723 = stdlib.ge(v711, v721);
        const v724 = v723 ? v711 : v721;
        const v725 = {
          accepted: false,
          amountRequested: v711,
          amountSet: v724,
          approvalsCount: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
          sumOfSetAmounts: v711
          };
        await stdlib.mapSet(map1, v688, v725);
        const v726 = true;
        await txn3.getOutput('CommunityMember_createClaim', 'v726', ctc3, v726);
        const txn4 = await (ctc.sendrecv({
          args: [v558, v559, v560, v578, v724],
          evt_cnt: 0,
          funcNum: 4,
          lct: v690,
          onlyIf: true,
          out_tys: [],
          pay: [v724, []],
          sim_p: (async (txn4) => {
            const sim_r = { txns: [], mapRefs: [], maps: [] };
            let sim_txn_ctr = stdlib.UInt_max;
            const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
            
            stdlib.simMapDupe(sim_r, 0, map0);
            stdlib.simMapDupe(sim_r, 1, map1);
            stdlib.simMapDupe(sim_r, 2, map2);
            
            const {data: [], secs: v745, time: v744, didSend: v356, from: v743 } = txn4;
            
            const v747 = stdlib.add(v578, v724);
            sim_r.txns.push({
              amt: v724,
              kind: 'to',
              tok: undefined /* Nothing */
              });
            const cv571 = v744;
            const cv578 = v747;
            
            await (async () => {
              const v571 = cv571;
              const v578 = cv578;
              
              if (await (async () => {
                
                return v560;})()) {
                sim_r.isHalt = false;
                }
              else {
                sim_r.txns.push({
                  kind: 'from',
                  to: v558,
                  tok: undefined /* Nothing */
                  });
                sim_r.txns.push({
                  kind: 'halt',
                  tok: undefined /* Nothing */
                  })
                sim_r.isHalt = true;
                }})();
            return sim_r;
            }),
          soloSend: true,
          timeoutAt: undefined /* mto */,
          tys: [ctc11, ctc2, ctc3, ctc2, ctc2],
          waitIfNotPresent: false
          }));
        const {data: [], secs: v745, time: v744, didSend: v356, from: v743 } = txn4;
        const v747 = stdlib.add(v578, v724);
        ;
        const v748 = stdlib.addressEq(v558, v743);
        stdlib.assert(v748, {
          at: './index.rsh:175:25:dot',
          fs: ['at ./index.rsh:147:13:application call to [unknown function] (defined at: ./index.rsh:147:13:function exp)'],
          msg: 'sender correct',
          who: 'Insurer'
          });
        const cv571 = v744;
        const cv578 = v747;
        
        v571 = cv571;
        v578 = cv578;
        
        continue;
        
        
        break;
        }
      case 'CommunityMember_payMonthlyFee0_53': {
        const v870 = v689[1];
        undefined /* setApiDetails */;
        const v877 = v870[stdlib.checkedBigNumberify('./index.rsh:134:14:spread', stdlib.UInt_max, '0')];
        const v878 = v877.mfee;
        const v886 = stdlib.add(v578, v878);
        ;
        const v933 = 'backend: API.CommunityMember.payMonthlyFee invoked.';
        stdlib.protect(ctc0, await interact.log(v933), {
          at: './index.rsh:138:37:application',
          fs: ['at ./index.rsh:138:37:application call to [unknown function] (defined at: ./index.rsh:138:37:function exp)', 'at ./index.rsh:138:37:application call to "liftedInteract" (defined at: ./index.rsh:138:37:application)', 'at ./index.rsh:137:13:application call to [unknown function] (defined at: ./index.rsh:137:13:function exp)'],
          msg: 'log',
          who: 'Insurer'
          });
        
        const v934 = true;
        await txn3.getOutput('CommunityMember_payMonthlyFee', 'v934', ctc3, v934);
        const v947 = stdlib.sub(v886, v878);
        ;
        const cv571 = v690;
        const cv578 = v947;
        
        v571 = cv571;
        v578 = cv578;
        
        continue;
        break;
        }
      case 'CommunityMember_registerMembership0_53': {
        const v1048 = v689[1];
        undefined /* setApiDetails */;
        const v1064 = stdlib.add(v578, v559);
        ;
        const v1128 = v1048[stdlib.checkedBigNumberify('./index.rsh:117:13:spread', stdlib.UInt_max, '0')];
        const v1129 = true;
        await txn3.getOutput('CommunityMember_registerMembership', 'v1129', ctc3, v1129);
        const v1141 = 'backend: API.CommunityMember.registerMembership ...';
        stdlib.protect(ctc0, await interact.log(v1141), {
          at: './index.rsh:123:37:application',
          fs: ['at ./index.rsh:123:37:application call to [unknown function] (defined at: ./index.rsh:123:37:function exp)', 'at ./index.rsh:123:37:application call to "liftedInteract" (defined at: ./index.rsh:123:37:application)', 'at ./index.rsh:120:13:application call to [unknown function] (defined at: ./index.rsh:120:13:function exp)'],
          msg: 'log',
          who: 'Insurer'
          });
        
        const v1143 = 'backend: Insurer.interact.saveNewMemberDetails invoked ...';
        stdlib.protect(ctc0, await interact.log(v1143), {
          at: './index.rsh:124:37:application',
          fs: ['at ./index.rsh:124:37:application call to [unknown function] (defined at: ./index.rsh:124:37:function exp)', 'at ./index.rsh:124:37:application call to "liftedInteract" (defined at: ./index.rsh:124:37:application)', 'at ./index.rsh:120:13:application call to [unknown function] (defined at: ./index.rsh:120:13:function exp)'],
          msg: 'log',
          who: 'Insurer'
          });
        
        stdlib.protect(ctc0, await interact.saveNewMemberDetails(v1128), {
          at: './index.rsh:125:54:application',
          fs: ['at ./index.rsh:125:54:application call to [unknown function] (defined at: ./index.rsh:125:54:function exp)', 'at ./index.rsh:125:54:application call to "liftedInteract" (defined at: ./index.rsh:125:54:application)', 'at ./index.rsh:120:13:application call to [unknown function] (defined at: ./index.rsh:120:13:function exp)'],
          msg: 'saveNewMemberDetails',
          who: 'Insurer'
          });
        
        const v1150 = 'backend: done.';
        stdlib.protect(ctc0, await interact.log(v1150), {
          at: './index.rsh:126:37:application',
          fs: ['at ./index.rsh:126:37:application call to [unknown function] (defined at: ./index.rsh:126:37:function exp)', 'at ./index.rsh:126:37:application call to "liftedInteract" (defined at: ./index.rsh:126:37:application)', 'at ./index.rsh:120:13:application call to [unknown function] (defined at: ./index.rsh:120:13:function exp)'],
          msg: 'log',
          who: 'Insurer'
          });
        
        const v1154 = stdlib.sub(v1064, v559);
        ;
        await stdlib.mapSet(map0, v688, null);
        const cv571 = v690;
        const cv578 = v1154;
        
        v571 = cv571;
        v578 = cv578;
        
        continue;
        break;
        }
      case 'CommunityMember_respondToClaim0_53': {
        const v1226 = v689[1];
        undefined /* setApiDetails */;
        ;
        const v1337 = v1226[stdlib.checkedBigNumberify('./index.rsh:179:14:spread', stdlib.UInt_max, '0')];
        const v1338 = v1337.claimant;
        const v1339 = true;
        await txn3.getOutput('CommunityMember_respondToClaim', 'v1339', ctc3, v1339);
        const v1350 = 'backend: API.CommunityMember.respondToClaim ...';
        stdlib.protect(ctc0, await interact.log(v1350), {
          at: './index.rsh:186:37:application',
          fs: ['at ./index.rsh:186:37:application call to [unknown function] (defined at: ./index.rsh:186:37:function exp)', 'at ./index.rsh:186:37:application call to "liftedInteract" (defined at: ./index.rsh:186:37:application)', 'at ./index.rsh:182:13:application call to [unknown function] (defined at: ./index.rsh:182:13:function exp)'],
          msg: 'log',
          who: 'Insurer'
          });
        
        const v1351 = v1337.accepted;
        if (v1351) {
          const v1352 = stdlib.protect(map1_ctc, await stdlib.mapRef(map1, v1338), null);
          let v1353;
          switch (v1352[0]) {
            case 'None': {
              const v1354 = v1352[1];
              v1353 = stdlib.checkedBigNumberify('./index.rsh:188:73:decimal', stdlib.UInt_max, '1');
              
              break;
              }
            case 'Some': {
              const v1355 = v1352[1];
              const v1356 = v1355.approvalsCount;
              v1353 = v1356;
              
              break;
              }
            }
          let v1358;
          switch (v1352[0]) {
            case 'None': {
              const v1359 = v1352[1];
              v1358 = stdlib.checkedBigNumberify('./index.rsh:189:73:decimal', stdlib.UInt_max, '0');
              
              break;
              }
            case 'Some': {
              const v1360 = v1352[1];
              const v1361 = v1360.sumOfSetAmounts;
              v1358 = v1361;
              
              break;
              }
            }
          let v1363;
          switch (v1352[0]) {
            case 'None': {
              const v1364 = v1352[1];
              v1363 = stdlib.checkedBigNumberify('./index.rsh:190:70:decimal', stdlib.UInt_max, '0');
              
              break;
              }
            case 'Some': {
              const v1365 = v1352[1];
              const v1366 = v1365.amountRequested;
              v1363 = v1366;
              
              break;
              }
            }
          let v1368;
          switch (v1352[0]) {
            case 'None': {
              const v1369 = v1352[1];
              v1368 = v1363;
              
              break;
              }
            case 'Some': {
              const v1370 = v1352[1];
              const v1371 = v1370.amountSet;
              v1368 = v1371;
              
              break;
              }
            }
          const v1372 = stdlib.lt(v1353, stdlib.checkedBigNumberify('./index.rsh:192:63:decimal', stdlib.UInt_max, '5'));
          const v1373 = stdlib.div(v1358, v1353);
          const v1374 = v1372 ? v1368 : v1373;
          const v1375 = stdlib.add(v1353, stdlib.checkedBigNumberify('./index.rsh:194:56:decimal', stdlib.UInt_max, '1'));
          const v1376 = {
            accepted: true,
            amountRequested: v1363,
            amountSet: v1374,
            approvalsCount: v1375,
            sumOfSetAmounts: v1358
            };
          await stdlib.mapSet(map1, v1338, v1376);
          const v1377 = stdlib.ge(v1353, stdlib.checkedBigNumberify('./index.rsh:201:41:decimal', stdlib.UInt_max, '5'));
          if (v1377) {
            await stdlib.mapSet(map2, v1338, undefined /* Nothing */);
            await stdlib.mapSet(map1, v1338, undefined /* Nothing */);
            stdlib.protect(ctc0, await interact.notifyFundedMember(v1338), {
              at: './index.rsh:211:60:application',
              fs: ['at ./index.rsh:211:60:application call to [unknown function] (defined at: ./index.rsh:211:60:function exp)', 'at ./index.rsh:211:60:application call to "liftedInteract" (defined at: ./index.rsh:211:60:application)', 'at ./index.rsh:182:13:application call to [unknown function] (defined at: ./index.rsh:182:13:function exp)'],
              msg: 'notifyFundedMember',
              who: 'Insurer'
              });
            
            const cv571 = v690;
            const cv578 = v578;
            
            v571 = cv571;
            v578 = cv578;
            
            continue;}
          else {
            const cv571 = v690;
            const cv578 = v578;
            
            v571 = cv571;
            v578 = cv578;
            
            continue;}}
        else {
          const cv571 = v690;
          const cv578 = v578;
          
          v571 = cv571;
          v578 = cv578;
          
          continue;}
        break;
        }
      case 'CommunityMember_stopContract0_53': {
        const v1404 = v689[1];
        undefined /* setApiDetails */;
        ;
        stdlib.protect(ctc0, await interact.stopContract(), {
          at: './index.rsh:244:46:application',
          fs: ['at ./index.rsh:244:46:application call to [unknown function] (defined at: ./index.rsh:244:46:function exp)', 'at ./index.rsh:244:46:application call to "liftedInteract" (defined at: ./index.rsh:244:46:application)', 'at ./index.rsh:240:13:application call to [unknown function] (defined at: ./index.rsh:240:13:function exp)'],
          msg: 'stopContract',
          who: 'Insurer'
          });
        
        const v1562 = true;
        await txn3.getOutput('CommunityMember_stopContract', 'v1562', ctc3, v1562);
        const cv571 = v690;
        const cv578 = v578;
        
        v571 = cv571;
        v578 = cv578;
        
        continue;
        break;
        }
      case 'CommunityMember_withDrawClaim0_53': {
        const v1582 = v689[1];
        undefined /* setApiDetails */;
        ;
        const v1748 = true;
        await txn3.getOutput('CommunityMember_withDrawClaim', 'v1748', ctc3, v1748);
        await stdlib.mapSet(map1, v688, undefined /* Nothing */);
        await stdlib.mapSet(map2, v688, undefined /* Nothing */);
        const cv571 = v690;
        const cv578 = v578;
        
        v571 = cv571;
        v578 = cv578;
        
        continue;
        break;
        }
      }
    
    }
  ;
  return;
  
  
  
  
  };
export async function CommunityMember_createClaim(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for CommunityMember_createClaim expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for CommunityMember_createClaim expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const step = await ctc.getCurrentStep()
  stdlib.assert(step == 4, 'API called in the wrong state. Currently in state: ' + step + ', expected:  [4]');
  if (step == 4) {return _CommunityMember_createClaim4(ctcTop, interact);}
  };
export async function CommunityMember_payMonthlyFee(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for CommunityMember_payMonthlyFee expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for CommunityMember_payMonthlyFee expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const step = await ctc.getCurrentStep()
  stdlib.assert(step == 4, 'API called in the wrong state. Currently in state: ' + step + ', expected:  [4]');
  if (step == 4) {return _CommunityMember_payMonthlyFee4(ctcTop, interact);}
  };
export async function CommunityMember_registerMembership(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for CommunityMember_registerMembership expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for CommunityMember_registerMembership expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const step = await ctc.getCurrentStep()
  stdlib.assert(step == 4, 'API called in the wrong state. Currently in state: ' + step + ', expected:  [4]');
  if (step == 4) {return _CommunityMember_registerMembership4(ctcTop, interact);}
  };
export async function CommunityMember_respondToClaim(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for CommunityMember_respondToClaim expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for CommunityMember_respondToClaim expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const step = await ctc.getCurrentStep()
  stdlib.assert(step == 4, 'API called in the wrong state. Currently in state: ' + step + ', expected:  [4]');
  if (step == 4) {return _CommunityMember_respondToClaim4(ctcTop, interact);}
  };
export async function CommunityMember_stopContract(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for CommunityMember_stopContract expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for CommunityMember_stopContract expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const step = await ctc.getCurrentStep()
  stdlib.assert(step == 4, 'API called in the wrong state. Currently in state: ' + step + ', expected:  [4]');
  if (step == 4) {return _CommunityMember_stopContract4(ctcTop, interact);}
  };
export async function CommunityMember_withDrawClaim(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for CommunityMember_withDrawClaim expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for CommunityMember_withDrawClaim expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const step = await ctc.getCurrentStep()
  stdlib.assert(step == 4, 'API called in the wrong state. Currently in state: ' + step + ', expected:  [4]');
  if (step == 4) {return _CommunityMember_withDrawClaim4(ctcTop, interact);}
  };
const _ALGO = {
  ABI: {
    impure: [`CommunityMember_createClaim((uint64,uint64,byte,uint64,uint64,uint64,uint64,uint64,uint64,byte[200]))byte`, `CommunityMember_payMonthlyFee((address,uint64))byte`, `CommunityMember_registerMembership((byte[60],byte[20],byte[60],uint64))byte`, `CommunityMember_respondToClaim((address,byte,uint64))byte`, `CommunityMember_stopContract()byte`, `CommunityMember_withDrawClaim()byte`],
    pure: [],
    sigs: [`CommunityMember_createClaim((uint64,uint64,byte,uint64,uint64,uint64,uint64,uint64,uint64,byte[200]))byte`, `CommunityMember_payMonthlyFee((address,uint64))byte`, `CommunityMember_registerMembership((byte[60],byte[20],byte[60],uint64))byte`, `CommunityMember_respondToClaim((address,byte,uint64))byte`, `CommunityMember_stopContract()byte`, `CommunityMember_withDrawClaim()byte`]
    },
  appApproval: `BiARAAEFBCAIiQIZPPejl9QM85WunA36i4H/D+WNk54J7fX92wsDKSImAwEAAQEAIjUAMRhBBZMqZEkiWzUBIQVbNQIxGSMSQQAKMQAoIQivZkIFXzYaABdJQQCnIjUEIzUGSSEJDEAATUkhCgxAADJJIQsMQAAWIQsSRDYaATX/gAECNP9QgXWvUEIA4CEKEkQ2GgE1/yk0/1CB4QGvUEIAyyEJEkQqNf+AAQQ0/1AhBq9QQgC3SSEMDEAAK0khDQxAABQhDRJEKjX/gAEFNP9QIQavUEIAlSEMEkQ2GgE1/yg0/1BCAIWBlbuVzgISRDYaATX/gAEDNP9QgeABr1BCAGo2GgIXNQQ2GgM2GgEXSSEODEADikklDEAATyUSRCQ0ARJENARJIhJMNAISEUQoZEk1A0lXACA1/4ExWzX+gASRJzTzsDT+iASWNP8xABJENP80AyEEWzQDVygBFzIGNAMhD1s0/ghCA89IJTQBEkQ0BEkiEkw0AhIRRChkSTUDSUpXACA1/yEEWzX+VygBFzX9IQ9bNfxJNQU1+4AELorGRTT7ULA0+yJVSSEODEABr0klDEAAcEkkDEAATSQSRIAJAAAAAAAABtQBsCk1BzEAKDEAiAP6IRCvNfpJVwABNPpQTFcjGVBmMQAoMQCIA+AhB681+lcAIzT6UGY0/zT+NP0yBjT8QgMvSIAJAAAAAAAABhoBsCk1BzT/NP40/TIGNPxCAxJINPtXASk1+jT6STX5VwAgNfiACQAAAAAAAAU7AbApNQc0+VcgARdBAQI0+IgDe1cBIkk19yJVQAAGIzX2QgAQNPdXASFJNfWBEVs19kIAADT3IlVAAAYiNfVCABA091cBIUk19CEHWzX1QgAANPciVUAABiI19EIADzT3VwEhSTXzIls19EIAADT3IlVAAAc09DXzQgAQNPdXASFJNfIhBVs180IAADT4KDT4iAL7KTT0FjT1NPYKNPM09iQMTRZQKVA09iMIFlA09RZQUDXySVcAATTyUExXIxlQZjT2JA9BADs0+Cg0+IgCviEHrzXyVwAjNPJQZjT4KDT4iAKqIRCvNfJJVwABNPJQTFcjGVBmNP80/jT9MgY0/EIB8zT/NP40/TIGNPxCAeY0/zT+NP0yBjT8QgHZSSMMQACSSYECDEAARUg0/ogCbYAJAAAAAAAABGkBsCk1B7EisgE0/rIII7IQNP+yB7MxACgxAIgCNyMiTFZmNP80/jT9MgY0/DT+CDT+CUIBh0g0+1cBKDX6NPohBFs1+TT5iAIagAkAAAAAAAADpgGwKTUHsSKyATT5sggjshA0/7IHszT/NP40/TIGNPw0+Qg0+QlCAUFINPsjIQZYNfo0+kk1+SJbNfgxACgxAIgBvyk0+VchCDT5VykIUDT5VzEIUFA191cAIzT3UGY0+YE5W0k19zT4STT3D0019jEAKDEAiAGKKTT4FjT2FlAoUCEFr1A0+BZQUDX1SVcAATT1UExXIxlQZoAJAAAAAAAAAtYBsCk1BzT/NP4WUDT9FlEHCFA0/BZQNPYWUChLAVcAOWdIJDUBMgY1AkIA9UkjDEAAPSMSRCM0ARJENARJIhJMNAISEUQoZEk1A1cAIDX/gASai5F0sDT/MQASRDT/NAMhBFs0A1coARcyBiJCAFdIgaCNBogA9iI0ARJENARJIhJMNAISEURJNQVJIls1/1cIARc1/oAEf/WC8DT/FlA0/hZRBwhQsDEANP8WUDT+FlEHCFAoSwFXAClnSCM1ATIGNQJCAFs1/zX+Nf01/DX7NP1BAB40+zT8FlApUDT/FlAoSwFXADFnSCU1ATIGNQJCAC6xIrIBNP+yCCOyEDT7sgezQgAAMRkkEkSxIrIBIrIII7IQMgmyCTIKsgezQgAFMRkiEkQqNAEWNAIWUGc0BkEACoAEFR98dTQHULA0AEkjCDIEEkQxFhJEI0MxGSISREL/3yI1ASI1AkL/w0kxGGFAAAVIIQiviShiiTQASUojCDUAOAcyChJEOBAjEkQ4CBJEiQ==`,
  appClear: `Bg==`,
  companionInfo: null,
  extraPages: 0,
  mapDataKeys: 1,
  mapDataSize: 60,
  stateKeys: 1,
  stateSize: 57,
  unsupported: [],
  version: 10,
  warnings: []
  };
const _ETH = {
  ABI: `[
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v559",
                "type": "uint256"
              },
              {
                "internalType": "bool",
                "name": "v560",
                "type": "bool"
              }
            ],
            "internalType": "struct T6",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T7",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "stateMutability": "payable",
    "type": "constructor"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "msg",
        "type": "uint256"
      }
    ],
    "name": "ReachError",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "_who",
        "type": "address"
      },
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v559",
                "type": "uint256"
              },
              {
                "internalType": "bool",
                "name": "v560",
                "type": "bool"
              }
            ],
            "internalType": "struct T6",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T7",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e0",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "_who",
        "type": "address"
      },
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T11",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e1",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "_who",
        "type": "address"
      },
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "components": [
                  {
                    "internalType": "enum _enum_T25",
                    "name": "which",
                    "type": "uint8"
                  },
                  {
                    "components": [
                      {
                        "components": [
                          {
                            "internalType": "uint256",
                            "name": "amountRequested",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "amountSet",
                            "type": "uint256"
                          },
                          {
                            "internalType": "bool",
                            "name": "accepted",
                            "type": "bool"
                          },
                          {
                            "internalType": "uint256",
                            "name": "approvalsCount",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "sumOfSetAmounts",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "insrPackageId",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "amountDue",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "matureBalance",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "fundLimit",
                            "type": "uint256"
                          },
                          {
                            "components": [
                              {
                                "internalType": "bytes32",
                                "name": "elem0",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem1",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem2",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem3",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem4",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem5",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes8",
                                "name": "elem6",
                                "type": "bytes8"
                              }
                            ],
                            "internalType": "struct T13",
                            "name": "description",
                            "type": "tuple"
                          }
                        ],
                        "internalType": "struct T14",
                        "name": "elem0",
                        "type": "tuple"
                      }
                    ],
                    "internalType": "struct T15",
                    "name": "_CommunityMember_createClaim0_53",
                    "type": "tuple"
                  },
                  {
                    "components": [
                      {
                        "components": [
                          {
                            "internalType": "address payable",
                            "name": "who",
                            "type": "address"
                          },
                          {
                            "internalType": "uint256",
                            "name": "mfee",
                            "type": "uint256"
                          }
                        ],
                        "internalType": "struct T16",
                        "name": "elem0",
                        "type": "tuple"
                      }
                    ],
                    "internalType": "struct T17",
                    "name": "_CommunityMember_payMonthlyFee0_53",
                    "type": "tuple"
                  },
                  {
                    "components": [
                      {
                        "components": [
                          {
                            "components": [
                              {
                                "internalType": "bytes32",
                                "name": "elem0",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes28",
                                "name": "elem1",
                                "type": "bytes28"
                              }
                            ],
                            "internalType": "struct T18",
                            "name": "fullName",
                            "type": "tuple"
                          },
                          {
                            "components": [
                              {
                                "internalType": "bytes20",
                                "name": "elem0",
                                "type": "bytes20"
                              }
                            ],
                            "internalType": "struct T19",
                            "name": "phone",
                            "type": "tuple"
                          },
                          {
                            "components": [
                              {
                                "internalType": "bytes32",
                                "name": "elem0",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes28",
                                "name": "elem1",
                                "type": "bytes28"
                              }
                            ],
                            "internalType": "struct T18",
                            "name": "email",
                            "type": "tuple"
                          },
                          {
                            "internalType": "uint256",
                            "name": "chosenInsurancePackage",
                            "type": "uint256"
                          }
                        ],
                        "internalType": "struct T20",
                        "name": "elem0",
                        "type": "tuple"
                      }
                    ],
                    "internalType": "struct T21",
                    "name": "_CommunityMember_registerMembership0_53",
                    "type": "tuple"
                  },
                  {
                    "components": [
                      {
                        "components": [
                          {
                            "internalType": "address payable",
                            "name": "claimant",
                            "type": "address"
                          },
                          {
                            "internalType": "bool",
                            "name": "accepted",
                            "type": "bool"
                          },
                          {
                            "internalType": "uint256",
                            "name": "setAmount",
                            "type": "uint256"
                          }
                        ],
                        "internalType": "struct T22",
                        "name": "elem0",
                        "type": "tuple"
                      }
                    ],
                    "internalType": "struct T23",
                    "name": "_CommunityMember_respondToClaim0_53",
                    "type": "tuple"
                  },
                  {
                    "internalType": "bool",
                    "name": "_CommunityMember_stopContract0_53",
                    "type": "bool"
                  },
                  {
                    "internalType": "bool",
                    "name": "_CommunityMember_withDrawClaim0_53",
                    "type": "bool"
                  }
                ],
                "internalType": "struct T25",
                "name": "v689",
                "type": "tuple"
              }
            ],
            "internalType": "struct T27",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct T28",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e3",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "_who",
        "type": "address"
      },
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "indexed": false,
        "internalType": "struct T11",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_e4",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bool",
        "name": "v0",
        "type": "bool"
      }
    ],
    "name": "_reach_oe_v1129",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bool",
        "name": "v0",
        "type": "bool"
      }
    ],
    "name": "_reach_oe_v1339",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bool",
        "name": "v0",
        "type": "bool"
      }
    ],
    "name": "_reach_oe_v1562",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bool",
        "name": "v0",
        "type": "bool"
      }
    ],
    "name": "_reach_oe_v1748",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bool",
        "name": "v0",
        "type": "bool"
      }
    ],
    "name": "_reach_oe_v726",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bool",
        "name": "v0",
        "type": "bool"
      }
    ],
    "name": "_reach_oe_v934",
    "type": "event"
  },
  {
    "stateMutability": "payable",
    "type": "fallback"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "amountRequested",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "amountSet",
            "type": "uint256"
          },
          {
            "internalType": "bool",
            "name": "accepted",
            "type": "bool"
          },
          {
            "internalType": "uint256",
            "name": "approvalsCount",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "sumOfSetAmounts",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "insrPackageId",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "amountDue",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "matureBalance",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "fundLimit",
            "type": "uint256"
          },
          {
            "components": [
              {
                "internalType": "bytes32",
                "name": "elem0",
                "type": "bytes32"
              },
              {
                "internalType": "bytes32",
                "name": "elem1",
                "type": "bytes32"
              },
              {
                "internalType": "bytes32",
                "name": "elem2",
                "type": "bytes32"
              },
              {
                "internalType": "bytes32",
                "name": "elem3",
                "type": "bytes32"
              },
              {
                "internalType": "bytes32",
                "name": "elem4",
                "type": "bytes32"
              },
              {
                "internalType": "bytes32",
                "name": "elem5",
                "type": "bytes32"
              },
              {
                "internalType": "bytes8",
                "name": "elem6",
                "type": "bytes8"
              }
            ],
            "internalType": "struct T13",
            "name": "description",
            "type": "tuple"
          }
        ],
        "internalType": "struct T14",
        "name": "_a0",
        "type": "tuple"
      }
    ],
    "name": "CommunityMember_createClaim",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "address payable",
            "name": "who",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "mfee",
            "type": "uint256"
          }
        ],
        "internalType": "struct T16",
        "name": "_a0",
        "type": "tuple"
      }
    ],
    "name": "CommunityMember_payMonthlyFee",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "bytes32",
                "name": "elem0",
                "type": "bytes32"
              },
              {
                "internalType": "bytes28",
                "name": "elem1",
                "type": "bytes28"
              }
            ],
            "internalType": "struct T18",
            "name": "fullName",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "bytes20",
                "name": "elem0",
                "type": "bytes20"
              }
            ],
            "internalType": "struct T19",
            "name": "phone",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "bytes32",
                "name": "elem0",
                "type": "bytes32"
              },
              {
                "internalType": "bytes28",
                "name": "elem1",
                "type": "bytes28"
              }
            ],
            "internalType": "struct T18",
            "name": "email",
            "type": "tuple"
          },
          {
            "internalType": "uint256",
            "name": "chosenInsurancePackage",
            "type": "uint256"
          }
        ],
        "internalType": "struct T20",
        "name": "_a0",
        "type": "tuple"
      }
    ],
    "name": "CommunityMember_registerMembership",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "address payable",
            "name": "claimant",
            "type": "address"
          },
          {
            "internalType": "bool",
            "name": "accepted",
            "type": "bool"
          },
          {
            "internalType": "uint256",
            "name": "setAmount",
            "type": "uint256"
          }
        ],
        "internalType": "struct T22",
        "name": "_a0",
        "type": "tuple"
      }
    ],
    "name": "CommunityMember_respondToClaim",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "CommunityMember_stopContract",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "CommunityMember_withDrawClaim",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "_reachCreationTime",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "_reachCurrentState",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "",
        "type": "bytes"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "_reachCurrentTime",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "addr",
        "type": "address"
      }
    ],
    "name": "_reachMap0Ref",
    "outputs": [
      {
        "components": [
          {
            "internalType": "enum _enum_T0",
            "name": "which",
            "type": "uint8"
          },
          {
            "internalType": "bool",
            "name": "_None",
            "type": "bool"
          },
          {
            "internalType": "bool",
            "name": "_Some",
            "type": "bool"
          }
        ],
        "internalType": "struct T0",
        "name": "res",
        "type": "tuple"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "addr",
        "type": "address"
      }
    ],
    "name": "_reachMap1Ref",
    "outputs": [
      {
        "components": [
          {
            "internalType": "enum _enum_T2",
            "name": "which",
            "type": "uint8"
          },
          {
            "internalType": "bool",
            "name": "_None",
            "type": "bool"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "amountRequested",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "amountSet",
                "type": "uint256"
              },
              {
                "internalType": "bool",
                "name": "accepted",
                "type": "bool"
              },
              {
                "internalType": "uint256",
                "name": "approvalsCount",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "sumOfSetAmounts",
                "type": "uint256"
              }
            ],
            "internalType": "struct T1",
            "name": "_Some",
            "type": "tuple"
          }
        ],
        "internalType": "struct T2",
        "name": "res",
        "type": "tuple"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "addr",
        "type": "address"
      }
    ],
    "name": "_reachMap2Ref",
    "outputs": [
      {
        "components": [
          {
            "internalType": "enum _enum_T4",
            "name": "which",
            "type": "uint8"
          },
          {
            "internalType": "bool",
            "name": "_None",
            "type": "bool"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "insrPackageId",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "amountDue",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "matureBalance",
                "type": "uint256"
              }
            ],
            "internalType": "struct T3",
            "name": "_Some",
            "type": "tuple"
          }
        ],
        "internalType": "struct T4",
        "name": "res",
        "type": "tuple"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T11",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_m1",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "components": [
              {
                "components": [
                  {
                    "internalType": "enum _enum_T25",
                    "name": "which",
                    "type": "uint8"
                  },
                  {
                    "components": [
                      {
                        "components": [
                          {
                            "internalType": "uint256",
                            "name": "amountRequested",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "amountSet",
                            "type": "uint256"
                          },
                          {
                            "internalType": "bool",
                            "name": "accepted",
                            "type": "bool"
                          },
                          {
                            "internalType": "uint256",
                            "name": "approvalsCount",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "sumOfSetAmounts",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "insrPackageId",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "amountDue",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "matureBalance",
                            "type": "uint256"
                          },
                          {
                            "internalType": "uint256",
                            "name": "fundLimit",
                            "type": "uint256"
                          },
                          {
                            "components": [
                              {
                                "internalType": "bytes32",
                                "name": "elem0",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem1",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem2",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem3",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem4",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes32",
                                "name": "elem5",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes8",
                                "name": "elem6",
                                "type": "bytes8"
                              }
                            ],
                            "internalType": "struct T13",
                            "name": "description",
                            "type": "tuple"
                          }
                        ],
                        "internalType": "struct T14",
                        "name": "elem0",
                        "type": "tuple"
                      }
                    ],
                    "internalType": "struct T15",
                    "name": "_CommunityMember_createClaim0_53",
                    "type": "tuple"
                  },
                  {
                    "components": [
                      {
                        "components": [
                          {
                            "internalType": "address payable",
                            "name": "who",
                            "type": "address"
                          },
                          {
                            "internalType": "uint256",
                            "name": "mfee",
                            "type": "uint256"
                          }
                        ],
                        "internalType": "struct T16",
                        "name": "elem0",
                        "type": "tuple"
                      }
                    ],
                    "internalType": "struct T17",
                    "name": "_CommunityMember_payMonthlyFee0_53",
                    "type": "tuple"
                  },
                  {
                    "components": [
                      {
                        "components": [
                          {
                            "components": [
                              {
                                "internalType": "bytes32",
                                "name": "elem0",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes28",
                                "name": "elem1",
                                "type": "bytes28"
                              }
                            ],
                            "internalType": "struct T18",
                            "name": "fullName",
                            "type": "tuple"
                          },
                          {
                            "components": [
                              {
                                "internalType": "bytes20",
                                "name": "elem0",
                                "type": "bytes20"
                              }
                            ],
                            "internalType": "struct T19",
                            "name": "phone",
                            "type": "tuple"
                          },
                          {
                            "components": [
                              {
                                "internalType": "bytes32",
                                "name": "elem0",
                                "type": "bytes32"
                              },
                              {
                                "internalType": "bytes28",
                                "name": "elem1",
                                "type": "bytes28"
                              }
                            ],
                            "internalType": "struct T18",
                            "name": "email",
                            "type": "tuple"
                          },
                          {
                            "internalType": "uint256",
                            "name": "chosenInsurancePackage",
                            "type": "uint256"
                          }
                        ],
                        "internalType": "struct T20",
                        "name": "elem0",
                        "type": "tuple"
                      }
                    ],
                    "internalType": "struct T21",
                    "name": "_CommunityMember_registerMembership0_53",
                    "type": "tuple"
                  },
                  {
                    "components": [
                      {
                        "components": [
                          {
                            "internalType": "address payable",
                            "name": "claimant",
                            "type": "address"
                          },
                          {
                            "internalType": "bool",
                            "name": "accepted",
                            "type": "bool"
                          },
                          {
                            "internalType": "uint256",
                            "name": "setAmount",
                            "type": "uint256"
                          }
                        ],
                        "internalType": "struct T22",
                        "name": "elem0",
                        "type": "tuple"
                      }
                    ],
                    "internalType": "struct T23",
                    "name": "_CommunityMember_respondToClaim0_53",
                    "type": "tuple"
                  },
                  {
                    "internalType": "bool",
                    "name": "_CommunityMember_stopContract0_53",
                    "type": "bool"
                  },
                  {
                    "internalType": "bool",
                    "name": "_CommunityMember_withDrawClaim0_53",
                    "type": "bool"
                  }
                ],
                "internalType": "struct T25",
                "name": "v689",
                "type": "tuple"
              }
            ],
            "internalType": "struct T27",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct T28",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_m3",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint256",
            "name": "time",
            "type": "uint256"
          },
          {
            "internalType": "bool",
            "name": "msg",
            "type": "bool"
          }
        ],
        "internalType": "struct T11",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "_reach_m4",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "stateMutability": "payable",
    "type": "receive"
  }
]`,
  Bytecode: `0x608060405260405162002af038038062002af0833981016040819052620000269162000218565b60008055436003556040805133815282516020808301919091528084015180518385015201511515606082015290517f2fbd261e209972b4f2c71dc3a4d7abb6c394221d99c98cee9b70fb6cc561e7fa9181900360800190a16200008d3415600762000111565b6040805160608082018352600060208084018281528486018381523380875288840180515184525184015115158252600194859055439094558651928301939093525194810194909452511515908301529060800160405160208183030381529060405260029080519060200190620001089291906200013b565b505050620002c7565b81620001375760405163100960cb60e01b81526004810182905260240160405180910390fd5b5050565b82805462000149906200028a565b90600052602060002090601f0160209004810192826200016d5760008555620001b8565b82601f106200018857805160ff1916838001178555620001b8565b82800160010185558215620001b8579182015b82811115620001b85782518255916020019190600101906200019b565b50620001c6929150620001ca565b5090565b5b80821115620001c65760008155600101620001cb565b604080519081016001600160401b03811182821017156200021257634e487b7160e01b600052604160045260246000fd5b60405290565b600081830360608112156200022c57600080fd5b62000236620001e1565b835181526040601f19830112156200024d57600080fd5b62000257620001e1565b915060208401518252604084015180151581146200027457600080fd5b6020838101919091528101919091529392505050565b600181811c908216806200029f57607f821691505b60208210811415620002c157634e487b7160e01b600052602260045260246000fd5b50919050565b61281980620002d76000396000f3fe6080604052600436106100e05760003560e01c80638323075711610084578063b5f82e6911610056578063b5f82e691461020b578063cadc2e7a1461021e578063ce02cf9e1461024b578063dbbb3fcc1461025e57005b806383230757146101ad5780638419045b146101c2578063a7661d54146101d5578063ab53f2c6146101e857005b80633bc5b7bf116100bd5780633bc5b7bf1461013857806357db2ac8146101655780637980f6c314610178578063817d57f31461018057005b80631442b017146100e95780631e93b0f1146101065780632c10a1591461012557005b366100e757005b005b6100f1610271565b60405190151581526020015b60405180910390f35b34801561011257600080fd5b506003545b6040519081526020016100fd565b6100e7610133366004611c1b565b6102be565b34801561014457600080fd5b50610158610153366004611c48565b6102e2565b6040516100fd9190611c92565b6100f1610173366004611e22565b6102f9565b6100f1610351565b34801561018c57600080fd5b506101a061019b366004611c48565b61039e565b6040516100fd9190611e3e565b3480156101b957600080fd5b50600154610117565b6100f16101d0366004611f80565b6103dd565b6100e76101e3366004611c1b565b610435565b3480156101f457600080fd5b506101fd610455565b6040516100fd929190611f9c565b6100e7610219366004611ff9565b6104f2565b34801561022a57600080fd5b5061023e610239366004611c48565b610512565b6040516100fd919061200c565b6100f16102593660046120a4565b610523565b6100f161026c3660046121ce565b61057a565b600061027b6117c5565b6102836117fa565b61028b611819565b600060a082015260048152604080516020808201909252828152908301526102b382846105ce565b505060800151919050565b6102c66117c5565b6102de6102d8368490038401846121eb565b826110cf565b5050565b6102ea6118f4565b6102f382611255565b92915050565b60006103036117c5565b61030b6117fa565b610313611819565b6040805160208082018352878252608084019190915260038352815180820190925282825283015261034582846105ce565b50506060015192915050565b600061035b6117c5565b6103636117fa565b61036b611819565b600060c0820152600581526040805160208082019092528281529083015261039382846105ce565b505060a00151919050565b6103d460408051606080820183526000808352602080840182905284519283018552818352820181905281840152909182015290565b6102f382611319565b60006103e76117c5565b6103ef6117fa565b6103f7611819565b6040805160208082018352878252606084019190915260028352815180820190925282825283015261042982846105ce565b50506040015192915050565b61043d6117c5565b6102de61044f368490038401846121eb565b82611417565b60006060600054600280805461046a9061224f565b80601f01602080910402602001604051908101604052809291908181526020018280546104969061224f565b80156104e35780601f106104b8576101008083540402835291602001916104e3565b820191906000526020600020905b8154815290600101906020018083116104c657829003601f168201915b50505050509050915091509091565b6104fa6117c5565b6102de61050c36849003840184612325565b826105ce565b61051a611907565b6102f3826115b6565b600061052d6117c5565b6105356117fa565b61053d611819565b60408051602080820183528782528383019190915260018352815180820190925282825283015261056e82846105ce565b50506020015192915050565b60006105846117c5565b61058c6117fa565b610594611819565b6040805160208082018352878252838101919091526000835281518082019092528282528301526105c582846105ce565b50505192915050565b6105de60046000541460126116a0565b81516105f99015806105f257508251600154145b60136116a0565b60008080556002805461060b9061224f565b80601f01602080910402602001604051908101604052809291908181526020018280546106379061224f565b80156106845780601f1061065957610100808354040283529160200191610684565b820191906000526020600020905b81548152906001019060200180831161066757829003601f168201915b505050505080602001905181019061069c91906123fe565b90506106a6611926565b7f1fd9b03d8d53bdf29e42edf1293db36470854e654217b991dd1186919a26216933856040516106d79291906124ec565b60405180910390a160006020850151515160058111156106f9576106f9611c6c565b14156109195760208085015151015181526107163415600c6116a0565b80515160a00151602080830180519290925282515160c00151825182015282515160e00151825160409081019190915233600090815260068352819020805460ff191660019081178255935180519482019490945591830151600283015591909101516003909101558051516101008101519051101561079d5780515161010001516107a2565b805151515b6040828101918252825151516060808501805192909252925181516020908101919091528151600090840181905282518501819052855151518351608090810191909152338252600580845291859020805460ff1990811660019081178355955180518784015580860151600284015580880151600384018054911515919093161790915596870151600482015595015194019390935590519081527f5b1b1a382bc1d940aa4d6ec6f90c1fc672e81e2185c6e72d09b8a5c7cc8de46a910160405180910390a160018084526040805160a0808201835260008083526020808401828152848601838152606080870185815260808089018781528d516001600160a01b0316808b528e88015187528e8c0151151586528e85015184528d8c01518252600590985543909b558951808701979097529351868a0152915115159085015251908301529451818301528351808203909201825260c0019092528151909261091292600292910190611a32565b50506110c9565b600160208501515151600581111561093357610933611c6c565b1415610a4f5760208085015151604001516080830181905251015161095b903414600d6116a0565b604051600181527ffd94339b5550d300ab0d4313d98e14b00b6f5f1f4fcd25fb87d326d2e4343ae69060200160405180910390a160016020808501919091528251608083015151909101516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156109dc573d6000803e3d6000fd5b506109e5611ab6565b825181516001600160a01b039091169052602080840151825182015260408085015183519015159101528082015143905260808301515101516060840151610a2e90829061268e565b610a3891906126a6565b6020808301510152610a49816116c5565b506110c9565b6002602085015151516005811115610a6957610a69611c6c565b1415610b6257610a8082602001513414600e6116a0565b604051600181527f9379db02f64b8fbbafd9a10c04120facb6634eeb8d06022d7e6a4e0d822cf74b9060200160405180910390a160016040808501919091528251602084015191516001600160a01b039091169180156108fc02916000818181858888f19350505050158015610afa573d6000803e3d6000fd5b50336000908152600460205260409020805462ff00ff19166001179055610b1f611ab6565b825181516001600160a01b039091169052602080840180518351830152604080860151845190151591015290820151439052516060840151610a2e90829061268e565b6003602085015151516005811115610b7c57610b7c611c6c565b1415610f84576020840151516080015160a0820152610b9d3415600f6116a0565b604051600181527ffb6f82c1c626740bded771e7b5a70c15066c87fb33e7e8b46894060faf264b939060200160405180910390a16001606084015260a0810151516020015115610f7c5760a08101515151610bf7906115b6565b60c08201819052516000906001811115610c1357610c13611c6c565b1415610c2557600160e0820152610c5f565b600160c0820151516001811115610c3e57610c3e611c6c565b1415610c5f5760c08101516040015161010082018190526060015160e08201525b600060c0820151516001811115610c7857610c78611c6c565b1415610c8b576000610120820152610cc6565b600160c0820151516001811115610ca457610ca4611c6c565b1415610cc65760c0810151604001516101408201819052608001516101208201525b600060c0820151516001811115610cdf57610cdf611c6c565b1415610cf2576000610160820152610d2a565b600160c0820151516001811115610d0b57610d0b611c6c565b1415610d2a5760c0810151604001516101808201819052516101608201525b600060c0820151516001811115610d4357610d43611c6c565b1415610d5a576101608101516101a0820152610d95565b600160c0820151516001811115610d7357610d73611c6c565b1415610d955760c0810151604001516101c08201819052602001516101a08201525b6101608101516101e08201515260e0810151600511610dc8578060e00151816101200151610dc391906126bd565b610dcf565b806101a001515b6101e08201805160200191909152516001604090910181905260e0820151610df7919061268e565b6101e0820180516060019190915261012082015190516080015260a081015151516001600160a01b0316600090815260056020526040902080546001919060ff1916828002179055506101e081015160a082015151516001600160a01b0316600090815260056020818152604092839020845160018201559084015160028201559183015160038301805460ff1916911515919091179055606083015160048301556080909201519082015560e082015110610f7c5760a08101805151516001600160a01b039081166000908152600660209081526040808320805461ffff19908116825560018083018690556002808401879055600393840187905597515151909616855260059384905291842080549092168255938101839055938401829055918301805460ff1916905560048301819055910155610f36611ab6565b825181516001600160a01b0390911690526020808401518251820152604080850151835190151591015280820180514390526060850151905190910152610a49816116c5565b610f36611ab6565b6004602085015151516005811115610f9e57610f9e611c6c565b1415610ff357610fb0341560106116a0565b604051600181527f363d1f0c008c4cd380c1fa738091a1339d09e2951f8b6647a7f0e155cf1f14ca9060200160405180910390a160016080840152610f36611ab6565b600560208501515151600581111561100d5761100d611c6c565b14156110c95761101f341560116116a0565b604051600181527f5b9c2256bf87017eedbe543df4072f1e75ed21f95819bcaee2b9d0fef861699b9060200160405180910390a1600160a08401819052336000908152600560208181526040808420805461ffff19908116825581870186905560028083018790556003808401805460ff19169055600484018890559290950186905560069093529084208054909216825593810183905590810182905590910155610f36611ab6565b50505050565b6110df600160005414600a6116a0565b81516110fa9015806110f357508251600154145b600b6116a0565b60008080556002805461110c9061224f565b80601f01602080910402602001604051908101604052809291908181526020018280546111389061224f565b80156111855780601f1061115a57610100808354040283529160200191611185565b820191906000526020600020905b81548152906001019060200180831161116857829003601f168201915b505050505080602001905181019061119d91906126df565b60408051338152855160208083019190915286015115158183015290519192507f400d21ea4e4a5e28b4ae5f0f476c201fc8036473fcf7c8cd252f38698020b4f1919081900360600190a16111f4341560086116a0565b805161120c906001600160a01b0316331460096116a0565b611214611ab6565b815181516001600160a01b0390911690526020808301518251820152604080840151835190151591015280820180514390525160009101526110c9816116c5565b61125d6118f4565b60016001600160a01b03831660009081526004602052604090205460ff16600181111561128c5761128c611c6c565b1415611309576001600160a01b038216600090815260046020526040908190208151606081019092528054829060ff1660018111156112cd576112cd611c6c565b60018111156112de576112de611c6c565b8152905460ff6101008204811615156020840152620100009091041615156040909101529050919050565b600080825260208201525b919050565b61134f60408051606080820183526000808352602080840182905284519283018552818352820181905281840152909182015290565b60016001600160a01b03831660009081526006602052604090205460ff16600181111561137e5761137e611c6c565b1415611309576001600160a01b038216600090815260066020526040908190208151606081019092528054829060ff1660018111156113bf576113bf611c6c565b60018111156113d0576113d0611c6c565b81528154610100900460ff16151560208083019190915260408051606081018252600185015481526002850154928101929092526003909301548184015291015292915050565b61142760056000541460166116a0565b815161144290158061143b57508251600154145b60176116a0565b6000808055600280546114549061224f565b80601f01602080910402602001604051908101604052809291908181526020018280546114809061224f565b80156114cd5780601f106114a2576101008083540402835291602001916114cd565b820191906000526020600020905b8154815290600101906020018083116114b057829003601f168201915b50505050508060200190518101906114e59190612756565b60408051338152855160208083019190915286015115158183015290519192507faa99e317c364fb804a6b7e67b51beee98735c62eb3df9d8182015e63bb190722919081900360600190a16115418160800151341460146116a0565b8051611559906001600160a01b0316331460156116a0565b611561611ab6565b815181516001600160a01b03909116905260208083015182518201526040808401518351901515910152810151439052608082015160608301516115a5919061268e565b60208083015101526110c9816116c5565b6115be611907565b60016001600160a01b03831660009081526005602052604090205460ff1660018111156115ed576115ed611c6c565b1415611309576001600160a01b038216600090815260056020526040908190208151606081019092528054829060ff16600181111561162e5761162e611c6c565b600181111561163f5761163f611c6c565b81528154610100900460ff90811615156020808401919091526040805160a081018252600186015481526002860154928101929092526003850154909216151581830152600484015460608201526005909301546080840152015292915050565b816102de5760405163100960cb60e01b81526004810182905260240160405180910390fd5b8051604001511561176a5760408051608080820183526000808352602080840182815284860183815260608087018581528951516001600160a01b03168089528a51860151855260018085528b8701518701518352600490975543909655885180860196909652925185890152905115159084015251828401528451808303909301835260a0909101909352805191926117659260029290910190611a32565b505050565b80515160208083015101516040516001600160a01b039092169181156108fc0291906000818181858888f193505050501580156117ab573d6000803e3d6000fd5b50600080805560018190556117c290600290611afc565b50565b6040805160c081018252600080825260208201819052918101829052606081018290526080810182905260a081019190915290565b604051806040016040528060008152602001611814611b36565b905290565b6040805160e081019091528060008152602001611834611b49565b8152602001611859604080516060810182526000602082018181529282015290815290565b81526020016118b46040805160e081018252600060a0820181815260c0830182905260208084019182528451808201865283815284860152845180860190955282855284018290526060830193909352608082015290815290565b81526020016118e060408051608081018252600060208201818152928201819052606082015290815290565b815260006020820181905260409091015290565b60408051606081019091528060006118e0565b6040805160608101825260008082526020820152908101611814611bd5565b60405180610200016040528061193a611b49565b815260200161196360405180606001604052806000815260200160008152602001600081525090565b815260200160008152602001611977611bd5565b815260200161199c604080516060810182526000602082018181529282015290815290565b81526020016119c860408051608081018252600060208201818152928201819052606082015290815290565b81526020016119d5611907565b8152602001600081526020016119e9611bd5565b8152602001600081526020016119fd611bd5565b815260200160008152602001611a11611bd5565b815260200160008152602001611a25611bd5565b8152602001611814611bd5565b828054611a3e9061224f565b90600052602060002090601f016020900481019282611a605760008555611aa6565b82601f10611a7957805160ff1916838001178555611aa6565b82800160010185558215611aa6579182015b82811115611aa6578251825591602001919060010190611a8b565b50611ab2929150611c06565b5090565b6040805160a08101825260009181018281526060820183905260808201929092529081908152602001611814604051806040016040528060008152602001600081525090565b508054611b089061224f565b6000825580601f10611b18575050565b601f0160209004906000526020600020908101906117c29190611c06565b6040518060200160405280611814611819565b6040805161016081018252600060208083018281528385018390526060808501849052608080860185905260a080870186905260c080880187905260e08089018890526101008901889052610120890188905289519081018a52878152958601879052978501869052918401859052830184905282018390529381019190915261014082015290815290565b6040518060a00160405280600081526020016000815260200160001515815260200160008152602001600081525090565b5b80821115611ab25760008155600101611c07565b600060408284031215611c2d57600080fd5b50919050565b6001600160a01b03811681146117c257600080fd5b600060208284031215611c5a57600080fd5b8135611c6581611c33565b9392505050565b634e487b7160e01b600052602160045260246000fd5b600281106117c2576117c2611c6c565b81516060820190611ca281611c82565b8083525060208301511515602083015260408301511515604083015292915050565b6040805190810167ffffffffffffffff81118282101715611cf557634e487b7160e01b600052604160045260246000fd5b60405290565b6040516020810167ffffffffffffffff81118282101715611cf557634e487b7160e01b600052604160045260246000fd5b60405160e0810167ffffffffffffffff81118282101715611cf557634e487b7160e01b600052604160045260246000fd5b604051610140810167ffffffffffffffff81118282101715611cf557634e487b7160e01b600052604160045260246000fd5b80151581146117c257600080fd5b803561131481611d8f565b600060608284031215611dba57600080fd5b6040516060810181811067ffffffffffffffff82111715611deb57634e487b7160e01b600052604160045260246000fd5b6040529050808235611dfc81611c33565b81526020830135611e0c81611d8f565b6020820152604092830135920191909152919050565b600060608284031215611e3457600080fd5b611c658383611da8565b815160a0820190611e4e81611c82565b82526020838101511515818401526040938401518051858501529081015160608401529092015160809091015290565b600060408284031215611e9057600080fd5b611e98611cc4565b823581529050602082013563ffffffff1981168114611eb657600080fd5b602082015292915050565b600081830360c0811215611ed457600080fd5b6040516080810181811067ffffffffffffffff82111715611f0557634e487b7160e01b600052604160045260246000fd5b604052915081611f158585611e7e565b81526020603f1983011215611f2957600080fd5b611f31611cfb565b915060408401356bffffffffffffffffffffffff1981168114611f5357600080fd5b825260208101829052611f698560608601611e7e565b604082015260a08401356060820152505092915050565b600060c08284031215611f9257600080fd5b611c658383611ec1565b82815260006020604081840152835180604085015260005b81811015611fd057858101830151858201606001528201611fb4565b81811115611fe2576000606083870101525b50601f01601f191692909201606001949350505050565b60006103e08284031215611c2d57600080fd5b815160e082019061201c81611c82565b8083525060208301511515602083015260408301518051604084015260208101516060840152604081015115156080840152606081015160a0840152608081015160c08401525092915050565b60006040828403121561207b57600080fd5b612083611cc4565b9050813561209081611c33565b808252506020820135602082015292915050565b6000604082840312156120b657600080fd5b611c658383612069565b600060e082840312156120d257600080fd5b6120da611d2c565b9050813581526020820135602082015260408201356040820152606082013560608201526080820135608082015260a082013560a082015260c082013567ffffffffffffffff60c01b8116811461213057600080fd5b60c082015292915050565b6000610200828403121561214e57600080fd5b612156611d5d565b9050813581526020820135602082015261217260408301611d9d565b6040820152606082013560608201526080820135608082015260a082013560a082015260c082013560c082015260e082013560e08201526101008083013581830152506101206121c4848285016120c0565b9082015292915050565b600061020082840312156121e157600080fd5b611c65838361213b565b6000604082840312156121fd57600080fd5b6040516040810181811067ffffffffffffffff8211171561222e57634e487b7160e01b600052604160045260246000fd5b60405282358152602083013561224381611d8f565b60208201529392505050565b600181811c9082168061226357607f821691505b60208210811415611c2d57634e487b7160e01b600052602260045260246000fd5b6000610200828403121561229757600080fd5b61229f611cfb565b90506122ab838361213b565b815292915050565b6000604082840312156122c557600080fd5b6122cd611cfb565b90506122ab8383612069565b600060c082840312156122eb57600080fd5b6122f3611cfb565b90506122ab8383611ec1565b60006060828403121561231157600080fd5b612319611cfb565b90506122ab8383611da8565b60008183036103e081121561233957600080fd5b612341611cc4565b833581526103c080601f198401121561235957600080fd5b612361611cfb565b925061236b611d2c565b60208601356006811061237d57600080fd5b815261238c8760408801612284565b602082015261239f8761024088016122b3565b60408201526123b28761028088016122d9565b60608201526123c58761034088016122ff565b60808201526123d76103a08701611d9d565b60a08201526123e7828701611d9d565b60c082015283525060208101919091529392505050565b60006080828403121561241057600080fd5b6040516080810181811067ffffffffffffffff8211171561244157634e487b7160e01b600052604160045260246000fd5b604052825161244f81611c33565b815260208381015190820152604083015161246981611d8f565b60408201526060928301519281019290925250919050565b805161249f8382518051825260209081015163ffffffff1916910152565b6bffffffffffffffffffffffff1960208201515116604084015260408101516124dc60608501828051825260209081015163ffffffff1916910152565b50606081015160a0840152505050565b6001600160a01b03831681528151602080830191909152820151518051610400830191906006811061252057612520611c6c565b806040850152506020810151518051606085015260208101516080850152604081015161255160a086018215159052565b5060608181015160c08681019190915260808084015160e08089019190915260a080860151610100808b019190915284870151610120808c0191909152928701516101408b01528601516101608a015294015180516101808901526020808201516101a08a01526040808301516101c08b0152828601516101e08b0152928201516102008a015294810151610220890152909101516001600160c01b0319166102408701528301515180516001600160a01b0316610260870152909101516102808501528101516126266102a0850182612481565b5060808101515180516001600160a01b031661036085015260208101511515610380850152604001516103a084015260a081015115156103c084015260c0015115156103e09092019190915292915050565b634e487b7160e01b600052601160045260246000fd5b600082198211156126a1576126a1612678565b500190565b6000828210156126b8576126b8612678565b500390565b6000826126da57634e487b7160e01b600052601260045260246000fd5b500490565b6000606082840312156126f157600080fd5b6040516060810181811067ffffffffffffffff8211171561272257634e487b7160e01b600052604160045260246000fd5b604052825161273081611c33565b815260208381015190820152604083015161274a81611d8f565b60408201529392505050565b600060a0828403121561276857600080fd5b60405160a0810181811067ffffffffffffffff8211171561279957634e487b7160e01b600052604160045260246000fd5b60405282516127a781611c33565b81526020838101519082015260408301516127c181611d8f565b604082015260608381015190820152608092830151928101929092525091905056fea264697066735822122064849577f86a149bc158f1b37db66cb3682a6d2c0ce265456fd1969caf38b91164736f6c634300080c0033`,
  BytecodeLen: 10992,
  Which: `oD`,
  version: 7,
  views: {
    }
  };
export const _stateSourceMap = {
  1: {
    at: './index.rsh:67:13:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  3: {
    at: './index.rsh:259:13:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  4: {
    at: './index.rsh:95:23:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  5: {
    at: './index.rsh:173:25:after expr stmt semicolon',
    fs: ['at ./index.rsh:147:13:application call to [unknown function] (defined at: ./index.rsh:147:13:function exp)'],
    msg: null,
    who: 'Module'
    }
  };
export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };
export const _Participants = {
  "CommunityMember_createClaim": CommunityMember_createClaim,
  "CommunityMember_payMonthlyFee": CommunityMember_payMonthlyFee,
  "CommunityMember_registerMembership": CommunityMember_registerMembership,
  "CommunityMember_respondToClaim": CommunityMember_respondToClaim,
  "CommunityMember_stopContract": CommunityMember_stopContract,
  "CommunityMember_withDrawClaim": CommunityMember_withDrawClaim,
  "Insurer": Insurer
  };
export const _APIs = {
  CommunityMember: {
    createClaim: CommunityMember_createClaim,
    payMonthlyFee: CommunityMember_payMonthlyFee,
    registerMembership: CommunityMember_registerMembership,
    respondToClaim: CommunityMember_respondToClaim,
    stopContract: CommunityMember_stopContract,
    withDrawClaim: CommunityMember_withDrawClaim
    }
  };
