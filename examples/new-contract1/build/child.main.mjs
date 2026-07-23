// Automatically generated with Reach 0.1.13 (2002b6c6*)
/* eslint-disable */
export const _version = '0.1.13';
export const _versionHash = '0.1.13 (2002b6c6*)';
export const _backendVersion = 27;

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
  const ctc0 = stdlib.T_UInt;
  
  return {
    infos: {
      },
    views: {
      3: [ctc0, ctc0]
      }
    };
  
  };
export function _getMaps(s) {
  const stdlib = s.reachStdlib;
  const ctc0 = stdlib.T_Tuple([]);
  return {
    mapDataTy: ctc0
    };
  };
export async function D(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for D expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for D expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Tuple([ctc0]);
  
  
  const v98 = stdlib.protect(ctc0, interact.y, 'for D\'s interact field y');
  
  const txn1 = await (ctc.sendrecv({
    args: [v98],
    evt_cnt: 1,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:14:5:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:14:5:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v102], secs: v104, time: v103, didSend: v27, from: v101 } = txn1;
      
      ;
      const v105 = stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:16:28:decimal', stdlib.UInt_max, '0');
      const v106 = v103;
      
      if (await (async () => {
        const v112 = stdlib.lt(v105, stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
        
        return v112;})()) {
        sim_r.isHalt = false;
        }
      else {
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
    tys: [ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v102], secs: v104, time: v103, didSend: v27, from: v101 } = txn1;
  ;
  let v105 = stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:16:28:decimal', stdlib.UInt_max, '0');
  let v106 = v103;
  
  let txn2 = txn1;
  while (await (async () => {
    const v112 = stdlib.lt(v105, stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
    
    return v112;})()) {
    const txn3 = await (ctc.recv({
      didSend: false,
      evt_cnt: 1,
      funcNum: 2,
      out_tys: [ctc1],
      timeoutAt: undefined /* mto */,
      waitIfNotPresent: false
      }));
    const {data: [v123], secs: v125, time: v124, didSend: v69, from: v122 } = txn3;
    undefined /* setApiDetails */;
    const v127 = v123[stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:19:10:spread', stdlib.UInt_max, '0')];
    ;
    const v130 = stdlib.safeAdd(v105, v102);
    const v131 = stdlib.safeAdd(v130, v127);
    await txn3.getOutput('f', 'v131', ctc0, v131);
    const v138 = stdlib.safeAdd(v105, stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
    const cv105 = v138;
    const cv106 = v124;
    
    v105 = cv105;
    v106 = cv106;
    
    txn2 = txn3;
    continue;
    
    }
  return;
  
  
  };
export async function _f3(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for _f3 expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for _f3 expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Tuple([ctc0]);
  const ctc2 = stdlib.T_Null;
  
  
  const [v102, v105] = await ctc.getState(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '3'), [ctc0, ctc0]);
  const v115 = stdlib.protect(ctc1, await interact.in(), {
    at: './examples/new-contract1/child.rsh:1:23:application',
    fs: ['at ./examples/new-contract1/child.rsh:19:20:application call to [unknown function] (defined at: ./examples/new-contract1/child.rsh:19:20:function exp)', 'at ./examples/new-contract1/child.rsh:19:20:application call to [unknown function] (defined at: ./examples/new-contract1/child.rsh:19:20:function exp)'],
    msg: 'in',
    who: 'f'
    });
  
  const txn1 = await (ctc.sendrecv({
    args: [v102, v105, v115],
    evt_cnt: 1,
    funcNum: 2,
    lct: stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:20:16:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v123], secs: v125, time: v124, didSend: v69, from: v122 } = txn1;
      
      sim_r.txns.push({
        kind: 'api',
        who: "f"
        });
      const v127 = v123[stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:19:10:spread', stdlib.UInt_max, '0')];
      ;
      const v130 = stdlib.safeAdd(v105, v102);
      const v131 = stdlib.safeAdd(v130, v127);
      const v132 = await txn1.getOutput('f', 'v131', ctc0, v131);
      
      const v138 = stdlib.safeAdd(v105, stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
      const v177 = v138;
      const v179 = stdlib.lt(v138, stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
      if (v179) {
        sim_r.isHalt = false;
        }
      else {
        sim_r.txns.push({
          kind: 'halt',
          tok: undefined /* Nothing */
          })
        sim_r.isHalt = true;
        }
      return sim_r;
      }),
    soloSend: false,
    timeoutAt: undefined /* mto */,
    tys: [ctc0, ctc0, ctc1],
    waitIfNotPresent: false
    }));
  const {data: [v123], secs: v125, time: v124, didSend: v69, from: v122 } = txn1;
  undefined /* setApiDetails */;
  const v127 = v123[stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:19:10:spread', stdlib.UInt_max, '0')];
  ;
  const v130 = stdlib.safeAdd(v105, v102);
  const v131 = stdlib.safeAdd(v130, v127);
  const v132 = await txn1.getOutput('f', 'v131', ctc0, v131);
  if (v69) {
    stdlib.protect(ctc2, await interact.out(v123, v132), {
      at: './examples/new-contract1/child.rsh:19:11:application',
      fs: ['at ./examples/new-contract1/child.rsh:19:11:application call to [unknown function] (defined at: ./examples/new-contract1/child.rsh:19:11:function exp)', 'at ./examples/new-contract1/child.rsh:21:10:application call to "k" (defined at: ./examples/new-contract1/child.rsh:20:23:function exp)', 'at ./examples/new-contract1/child.rsh:20:23:application call to [unknown function] (defined at: ./examples/new-contract1/child.rsh:20:23:function exp)'],
      msg: 'out',
      who: 'f'
      });
    }
  else {
    }
  
  const v138 = stdlib.safeAdd(v105, stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
  const v177 = v138;
  const v179 = stdlib.lt(v138, stdlib.checkedBigNumberify('./examples/new-contract1/child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
  if (v179) {
    return;
    }
  else {
    return;
    }
  
  
  };
export async function f(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for f expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for f expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const step = await ctc.getCurrentStep()
  if (step == 3) {return _f3(ctcTop, interact);}
  throw stdlib.apiStateMismatchError({ _stateSourceMap }, [stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '3')], stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, step))
  };
const _ALGO = {
  ABI: {
    impure: [`_reachp_0((uint64,uint64))void`, `_reachp_2((uint64,(uint64)))void`, `f(uint64)uint64`],
    pure: [],
    sigs: [`_reachp_0((uint64,uint64))void`, `_reachp_2((uint64,(uint64)))void`, `f(uint64)uint64`]
    },
  GlobalNumByteSlice: 2,
  GlobalNumUint: 0,
  LocalNumByteSlice: 0,
  LocalNumUint: 0,
  appApproval: `CCAFAAEIAgMmAgABADEYQQEUKGRJIls1ASRbNQIpZIIDBA/jBWkEUlb9rARuqG9INhoAjgMBEgDMAOAANA80EAg0DRcINQuACAAAAAAAAACDNAsWULA0CxY1BDQPIwgyBjUONQ80DyUMQQDqNBAWNA8WUCEEMgY1AjUBKUxXABBnKDQBFjQCFlBnMRkiEkSIARk0A0AACoAEFR98dTQEULAjQySvNAsWUDULIQQ0ARJESSJbNRAkWzUPNAsiWzUMNAtXCAg1DYAEleaaojQMFlA0DVCwNAyIAN1C/180CyJbNQw0CyRbNRCABKzRH8M0DBZQNBAWULA0DIgAuiIyBjUONQ9C/16IAJ2BoI0GNAYINQY2GgE1C0L/wogAiTYaATULQv+FIjE0EkQlMTUSRCIxNhJEIjE3EkSIAGqBEK8iIkL/MjEZIhJEQv9KNhoBFzULQv9NMRmBBRJEiABMIjIKMgmIAFpC/ygisgEjshCyB7IIs4lIiUwJSTUGMgmIAEWJCUlB/+5JNQYxFjQAIwhJNQAJRwI4BzIKEkQ4ECMSRDgIEkSJIzUDiTQGNAdKD0H/xEL/zEkiEkw0AhIRRImxsglC/6OxQv+f`,
  appApprovalMap: {
    0: `2`,
    1: `2`,
    10: `2`,
    100: `44`,
    101: `46`,
    102: `46`,
    103: `47`,
    104: `48`,
    105: `49`,
    106: `49`,
    107: `49`,
    108: `51`,
    109: `51`,
    11: `2`,
    110: `52`,
    111: `53`,
    112: `53`,
    113: `54`,
    114: `55`,
    115: `56`,
    116: `56`,
    117: `57`,
    118: `57`,
    119: `59`,
    12: `2`,
    120: `59`,
    121: `60`,
    122: `60`,
    123: `61`,
    124: `62`,
    125: `63`,
    126: `63`,
    127: `63`,
    128: `64`,
    129: `65`,
    13: `4`,
    130: `66`,
    131: `66`,
    132: `67`,
    133: `68`,
    134: `68`,
    135: `69`,
    136: `70`,
    137: `71`,
    138: `72`,
    139: `72`,
    14: `4`,
    140: `73`,
    141: `74`,
    142: `75`,
    143: `77`,
    144: `77`,
    145: `77`,
    146: `79`,
    147: `79`,
    148: `80`,
    149: `80`,
    15: `5`,
    150: `80`,
    151: `82`,
    152: `82`,
    153: `82`,
    154: `82`,
    155: `82`,
    156: `82`,
    157: `83`,
    158: `83`,
    159: `84`,
    16: `5`,
    160: `85`,
    161: `87`,
    162: `88`,
    163: `90`,
    164: `91`,
    165: `92`,
    166: `92`,
    167: `93`,
    168: `94`,
    169: `95`,
    17: `5`,
    170: `95`,
    171: `97`,
    172: `97`,
    173: `98`,
    174: `98`,
    175: `99`,
    176: `100`,
    177: `102`,
    178: `103`,
    179: `104`,
    18: `6`,
    180: `105`,
    181: `105`,
    182: `106`,
    183: `107`,
    184: `108`,
    185: `108`,
    186: `109`,
    187: `109`,
    188: `110`,
    189: `111`,
    19: `7`,
    190: `112`,
    191: `112`,
    192: `113`,
    193: `113`,
    194: `114`,
    195: `114`,
    196: `114`,
    197: `115`,
    198: `115`,
    199: `116`,
    2: `2`,
    20: `8`,
    200: `116`,
    201: `116`,
    202: `116`,
    203: `116`,
    204: `116`,
    205: `117`,
    206: `117`,
    207: `118`,
    208: `119`,
    209: `120`,
    21: `9`,
    210: `120`,
    211: `121`,
    212: `122`,
    213: `124`,
    214: `124`,
    215: `125`,
    216: `125`,
    217: `125`,
    218: `126`,
    219: `126`,
    22: `10`,
    220: `126`,
    221: `128`,
    222: `128`,
    223: `129`,
    224: `130`,
    225: `131`,
    226: `131`,
    227: `132`,
    228: `132`,
    229: `133`,
    23: `11`,
    230: `134`,
    231: `135`,
    232: `135`,
    233: `136`,
    234: `136`,
    235: `136`,
    236: `136`,
    237: `136`,
    238: `136`,
    239: `137`,
    24: `11`,
    240: `137`,
    241: `138`,
    242: `139`,
    243: `140`,
    244: `140`,
    245: `141`,
    246: `142`,
    247: `143`,
    248: `145`,
    249: `145`,
    25: `12`,
    250: `146`,
    251: `146`,
    252: `146`,
    253: `147`,
    254: `148`,
    255: `148`,
    256: `149`,
    257: `149`,
    258: `150`,
    259: `150`,
    26: `13`,
    260: `151`,
    261: `151`,
    262: `151`,
    263: `153`,
    264: `153`,
    265: `153`,
    266: `154`,
    267: `154`,
    268: `154`,
    269: `154`,
    27: `14`,
    270: `156`,
    271: `156`,
    272: `157`,
    273: `158`,
    274: `158`,
    275: `159`,
    276: `159`,
    277: `159`,
    278: `160`,
    279: `160`,
    28: `14`,
    280: `161`,
    281: `161`,
    282: `161`,
    283: `163`,
    284: `163`,
    285: `163`,
    286: `164`,
    287: `164`,
    288: `164`,
    289: `165`,
    29: `15`,
    290: `165`,
    291: `166`,
    292: `166`,
    293: `166`,
    294: `168`,
    295: `169`,
    296: `169`,
    297: `170`,
    298: `171`,
    299: `172`,
    3: `2`,
    30: `16`,
    300: `173`,
    301: `173`,
    302: `174`,
    303: `175`,
    304: `176`,
    305: `177`,
    306: `177`,
    307: `178`,
    308: `179`,
    309: `180`,
    31: `18`,
    310: `181`,
    311: `181`,
    312: `182`,
    313: `183`,
    314: `184`,
    315: `184`,
    316: `184`,
    317: `185`,
    318: `185`,
    319: `186`,
    32: `18`,
    320: `187`,
    321: `188`,
    322: `189`,
    323: `189`,
    324: `189`,
    325: `191`,
    326: `191`,
    327: `192`,
    328: `193`,
    329: `194`,
    33: `18`,
    330: `196`,
    331: `196`,
    332: `196`,
    333: `198`,
    334: `198`,
    335: `198`,
    336: `199`,
    337: `200`,
    338: `200`,
    339: `201`,
    34: `18`,
    340: `201`,
    341: `201`,
    342: `204`,
    343: `204`,
    344: `205`,
    345: `205`,
    346: `206`,
    347: `207`,
    348: `209`,
    349: `209`,
    35: `18`,
    350: `209`,
    351: `211`,
    352: `212`,
    353: `212`,
    354: `213`,
    355: `213`,
    356: `214`,
    357: `214`,
    358: `214`,
    359: `215`,
    36: `18`,
    360: `215`,
    361: `215`,
    362: `217`,
    363: `218`,
    364: `218`,
    365: `219`,
    366: `220`,
    367: `220`,
    368: `221`,
    369: `221`,
    37: `18`,
    370: `222`,
    371: `222`,
    372: `223`,
    373: `224`,
    374: `226`,
    375: `227`,
    376: `229`,
    377: `230`,
    378: `231`,
    379: `232`,
    38: `18`,
    380: `232`,
    381: `233`,
    382: `233`,
    383: `234`,
    384: `234`,
    385: `234`,
    386: `235`,
    387: `237`,
    388: `238`,
    389: `239`,
    39: `18`,
    390: `239`,
    391: `239`,
    392: `240`,
    393: `241`,
    394: `241`,
    395: `244`,
    396: `244`,
    397: `245`,
    398: `245`,
    399: `246`,
    4: `2`,
    40: `18`,
    400: `247`,
    401: `248`,
    402: `249`,
    403: `249`,
    404: `250`,
    405: `251`,
    406: `251`,
    407: `252`,
    408: `252`,
    409: `253`,
    41: `18`,
    410: `253`,
    411: `254`,
    412: `255`,
    413: `256`,
    414: `256`,
    415: `257`,
    416: `258`,
    417: `259`,
    418: `260`,
    419: `260`,
    42: `18`,
    420: `261`,
    421: `262`,
    422: `263`,
    423: `265`,
    424: `266`,
    425: `266`,
    426: `267`,
    427: `269`,
    428: `269`,
    429: `270`,
    43: `18`,
    430: `270`,
    431: `271`,
    432: `272`,
    433: `273`,
    434: `273`,
    435: `273`,
    436: `274`,
    437: `274`,
    438: `274`,
    439: `276`,
    44: `18`,
    440: `277`,
    441: `278`,
    442: `279`,
    443: `280`,
    444: `280`,
    445: `281`,
    446: `282`,
    447: `283`,
    448: `284`,
    449: `286`,
    45: `18`,
    450: `287`,
    451: `287`,
    452: `288`,
    453: `288`,
    454: `288`,
    455: `290`,
    456: `291`,
    46: `18`,
    47: `18`,
    48: `19`,
    49: `19`,
    5: `2`,
    50: `19`,
    51: `20`,
    52: `20`,
    53: `20`,
    54: `20`,
    55: `20`,
    56: `20`,
    57: `20`,
    58: `20`,
    59: `22`,
    6: `2`,
    60: `24`,
    61: `24`,
    62: `25`,
    63: `25`,
    64: `26`,
    65: `27`,
    66: `27`,
    67: `28`,
    68: `29`,
    69: `30`,
    7: `2`,
    70: `30`,
    71: `31`,
    72: `31`,
    73: `31`,
    74: `31`,
    75: `31`,
    76: `31`,
    77: `31`,
    78: `31`,
    79: `31`,
    8: `2`,
    80: `31`,
    81: `32`,
    82: `32`,
    83: `33`,
    84: `34`,
    85: `35`,
    86: `36`,
    87: `36`,
    88: `37`,
    89: `38`,
    9: `2`,
    90: `38`,
    91: `39`,
    92: `39`,
    93: `40`,
    94: `41`,
    95: `42`,
    96: `42`,
    97: `43`,
    98: `43`,
    99: `44`
    },
  appClear: `CA==`,
  appClearMap: {
    },
  companionInfo: null,
  extraPages: 0,
  stateKeys: 1,
  stateSize: 16,
  unsupported: [],
  version: 13,
  warnings: []
  };
const _ETH = {
  ABI: `[{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"internalType":"struct T2","name":"v187","type":"tuple"}],"stateMutability":"payable","type":"constructor"},{"inputs":[{"internalType":"uint256","name":"msg","type":"uint256"}],"name":"ReachError","type":"error"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T0","name":"elem1","type":"tuple"}],"indexed":false,"internalType":"struct T1","name":"_a","type":"tuple"}],"name":"_reach_e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v0","type":"uint256"}],"name":"_reach_oe_v131","type":"event"},{"stateMutability":"payable","type":"fallback"},{"inputs":[],"name":"_reachCreationTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentState","outputs":[{"internalType":"uint256","name":"","type":"uint256"},{"internalType":"bytes","name":"","type":"bytes"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T0","name":"elem1","type":"tuple"}],"internalType":"struct T1","name":"v190","type":"tuple"}],"name":"_reachp_2","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"internalType":"uint256","name":"v184","type":"uint256"}],"name":"f","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"payable","type":"function"},{"stateMutability":"payable","type":"receive"}]`,
  Bytecode: `0x608080604052604081610af0803803809161001a828561032a565b8339810103126102f4576000602060405192610035846102f9565b80518452015191602081019283528180554360035561005261034d565b5060ff600454166102e0577f6de3f97962105ba8e929dd0da178e54f00336c9ea6154699025bad3d4f17547f60606040513381528351602082015285516040820152a15180159081156102d4575b50156102c057346102ac57604051606081016001600160401b03811182821017610298576040908152828252602080830184815283830185815295518452439095529051916100ee836102f9565b8383528382840152519384835251910190815260038255436001556040519260208401525160408301526040825261012760608361032a565b8151906001600160401b03821161028457600254600181811c9116801561027a575b602082101461026657601f8111610203575b506020601f83116001146101a35782938293610198575b5050508160011b916000199060031b1c1916176002555b60405161078990816103678239f35b015191503880610172565b601f1983169360028352818320925b8581106101eb575083600195106101d2575b505050811b01600255610189565b015160001960f88460031b161c191690553880806101c4565b919260206001819286850151815501940192016101b2565b600282527f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace601f840160051c8101916020851061025c575b601f0160051c01905b818110610251575061015b565b828155600101610244565b909150819061023b565b634e487b7160e01b82526022600452602482fd5b90607f1690610149565b634e487b7160e01b81526041600452602490fd5b634e487b7160e01b83526041600452602483fd5b63100960cb60e01b81526009600452602490fd5b63100960cb60e01b81526008600452602490fd5b905060015414386100a0565b63100960cb60e01b82526007600452602482fd5b600080fd5b604081019081106001600160401b0382111761031457604052565b634e487b7160e01b600052604160045260246000fd5b601f909101601f19168101906001600160401b0382119082101761031457604052565b6040519061035a826102f9565b6000602083828152015256fe608080604052600436101561001a575b50361561001857005b005b60003560e01c9081631e93b0f1146101e15750806346ff359c146101775780638323075714610159578063ab53f2c6146100e95763b3de648b1461005e573861000f565b60203660031901126100e457602080604051610079816101fc565b600081526000828201526100da816040516100938161022d565b60405161009f8161022d565b8082526004359052604051906100b4826101fc565b60008252858201906040516100c88161022d565b6000815282526000835251905261034c565b0151604051908152f35b600080fd5b346100e45760003660031901126100e4576000546101056102a5565b604051918252604060208301528181519182604083015260005b8381106101415750508160006060809484010152601f80199101168101030190f35b6020828201810151606087840101528593500161011f565b346100e45760003660031901126100e4576020600154604051908152f35b60403660031901126100e4576040516000610191826101fc565b8082526020820152604051906101a6826101fc565b600435825260203660231901126100e4576101d6916040516101c78161022d565b6024358152602082015261034c565b602060405160008152f35b346100e45760003660031901126100e4576020906003548152f35b604081019081106001600160401b0382111761021757604052565b634e487b7160e01b600052604160045260246000fd5b602081019081106001600160401b0382111761021757604052565b601f909101601f19168101906001600160401b0382119082101761021757604052565b90600182811c9216801561029b575b602083101461028557565b634e487b7160e01b600052602260045260246000fd5b91607f169161027a565b60405190600082600254916102b98361026b565b808352926001811690811561032d57506001146102df575b6102dd92500383610248565b565b5060026000908152909160008051602061075d8339815191525b8183106103115750509060206102dd928201016102d1565b60209193508060019154838589010152019101909184926102f9565b602092506102dd94915060ff191682840152151560051b8201016102d1565b91906040519061035b8261022d565b60008252600360005403610705576103716102a5565b906040828051810103126100e4576000926040805193610390856101fc565b602081015185520151956020840196875260ff600454166106f15760405190338252805160208301527f263ae805ef0ac75eacb24e0a5ab78e31f247f0b08fe9d5cbf5188647933698b8606060208301938451516040820152a15180159081156106e5575b50156106d157346106bd57907fb1885c0fe128efbc823b7323770c75a9e7e8e72714fc480be1ce86ddb4b4304f602061043f81946104368b5189519061071b565b9051519061071b565b808452604051908152a15191015260405190606082016001600160401b038111838210176106a95760405282825260208201948386526040830191848352518352519060018201918281116106955782106106915781865243905260021115610603576020604051916104b1836101fc565b838352838284015251948583525191019081526003825543600155604051936020850152516040840152604083526104ea606084610248565b8251906001600160401b0382116105ef5761050660025461026b565b601f81116105a8575b506020601f831160011461054a57829394829361053f575b5050508160011b916000199060031b1c191617600255565b015191503880610527565b601f1983169460028352818320925b8681106105905750836001959610610577575b505050811b01600255565b015160001960f88460031b161c1916905538808061056c565b91926020600181928685015181550194019201610559565b600282526105df9060008051602061075d833981519152601f850160051c810191602086106105e5575b601f0160051c0190610745565b3861050f565b90915081906105d2565b634e487b7160e01b81526041600452602490fd5b508080929350558060015561061960025461026b565b80610622575050565b601f81116001146106335750600255565b6002825261067790601f0160051c60008051602061075d833981519152017f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5acf610745565b600280825281905560008051602061075d83398151915255565b8380fd5b634e487b7160e01b85526011600452602485fd5b634e487b7160e01b84526041600452602484fd5b63100960cb60e01b8552600d600452602485fd5b63100960cb60e01b8552600c600452602485fd5b905060015414386103f5565b63100960cb60e01b8552600b600452602485fd5b63100960cb60e01b600052600a60045260246000fd5b919082019182811161072f5782106100e457565b634e487b7160e01b600052601160045260246000fd5b818110610750575050565b6000815560010161074556fe405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5acea164736f6c634300081a000a`,
  BytecodeLen: 2800,
  version: 9,
  views: {
    }
  };
export const _stateSourceMap = {
  2: {
    at: './examples/new-contract1/child.rsh:26:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  3: {
    at: './examples/new-contract1/child.rsh:16:27:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    }
  };
export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
  };
export const _Participants = {
  "D": D,
  "f": f
  };
export const _APIs = {
  f: f
  };
