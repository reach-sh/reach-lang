// Automatically generated with Reach 0.1.13
/* eslint-disable */
export const _version = '0.1.13';
export const _versionHash = '0.1.13';
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
  const ctc0 = stdlib.T_Address;
  const ctc1 = stdlib.T_UInt;
  
  return {
    infos: {
      },
    views: {
      1: [ctc0, ctc1],
      2: [ctc0, ctc1]
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
export async function Alice(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for Alice expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Alice expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '128'));
  const ctc1 = stdlib.T_UInt;
  const ctc2 = stdlib.T_Address;
  
  
  const v64 = stdlib.protect(ctc0, interact.info, 'for Alice\'s interact field info');
  const v65 = stdlib.protect(ctc1, interact.request, 'for Alice\'s interact field request');
  
  const txn1 = await (ctc.sendrecv({
    args: [v65],
    evt_cnt: 1,
    funcNum: 0,
    lct: stdlib.checkedBigNumberify('./index.rsh:17:5:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./index.rsh:17:5:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v69], secs: v71, time: v70, didSend: v28, from: v68 } = txn1;
      
      ;
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc1],
    waitIfNotPresent: false
    }));
  const {data: [v69], secs: v71, time: v70, didSend: v28, from: v68 } = txn1;
  ;
  const txn2 = await (ctc.recv({
    didSend: false,
    evt_cnt: 0,
    funcNum: 1,
    out_tys: [],
    timeoutAt: undefined /* mto */,
    waitIfNotPresent: false
    }));
  const {data: [], secs: v76, time: v75, didSend: v37, from: v74 } = txn2;
  ;
  const txn3 = await (ctc.sendrecv({
    args: [v68, v69, v64],
    evt_cnt: 1,
    funcNum: 2,
    lct: v75,
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./index.rsh:27:5:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn3) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v82], secs: v84, time: v83, didSend: v46, from: v81 } = txn3;
      
      ;
      sim_r.txns.push({
        kind: 'from',
        to: v68,
        tok: undefined /* Nothing */
        });
      sim_r.txns.push({
        kind: 'halt',
        tok: undefined /* Nothing */
        })
      sim_r.isHalt = true;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc2, ctc1, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [v82], secs: v84, time: v83, didSend: v46, from: v81 } = txn3;
  ;
  const v85 = stdlib.addressEq(v68, v81);
  stdlib.assert(v85, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
    });
  ;
  return;
  
  
  
  
  
  
  };
export async function Bob(ctcTop, interact) {
  if (typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined) {
    return Promise.reject(new Error(`The backend for Bob expects to receive a contract as its first argument.`));}
  if (typeof(interact) !== 'object') {
    return Promise.reject(new Error(`The backend for Bob expects to receive an interact object as its second argument.`));}
  const ctc = ctcTop._initialize();
  const stdlib = ctc.stdlib;
  const ctc0 = stdlib.T_UInt;
  const ctc1 = stdlib.T_Null;
  const ctc2 = stdlib.T_Bytes(stdlib.checkedBigNumberify('<builtin>', stdlib.UInt_max, '128'));
  const ctc3 = stdlib.T_Address;
  
  
  const txn1 = await (ctc.recv({
    didSend: false,
    evt_cnt: 1,
    funcNum: 0,
    out_tys: [ctc0],
    timeoutAt: undefined /* mto */,
    waitIfNotPresent: false
    }));
  const {data: [v69], secs: v71, time: v70, didSend: v28, from: v68 } = txn1;
  ;
  stdlib.protect(ctc1, await interact.want(v69), {
    at: './index.rsh:21:18:application',
    fs: ['at ./index.rsh:20:9:application call to [unknown function] (defined at: ./index.rsh:20:13:function exp)'],
    msg: 'want',
    who: 'Bob'
    });
  
  const txn2 = await (ctc.sendrecv({
    args: [v68, v69],
    evt_cnt: 0,
    funcNum: 1,
    lct: v70,
    onlyIf: true,
    out_tys: [],
    pay: [v69, []],
    sim_p: (async (txn2) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [], secs: v76, time: v75, didSend: v37, from: v74 } = txn2;
      
      sim_r.txns.push({
        amt: v69,
        kind: 'to',
        tok: undefined /* Nothing */
        });
      sim_r.isHalt = false;
      
      return sim_r;
      }),
    soloSend: true,
    timeoutAt: undefined /* mto */,
    tys: [ctc3, ctc0],
    waitIfNotPresent: false
    }));
  const {data: [], secs: v76, time: v75, didSend: v37, from: v74 } = txn2;
  ;
  const txn3 = await (ctc.recv({
    didSend: false,
    evt_cnt: 1,
    funcNum: 2,
    out_tys: [ctc2],
    timeoutAt: undefined /* mto */,
    waitIfNotPresent: false
    }));
  const {data: [v82], secs: v84, time: v83, didSend: v46, from: v81 } = txn3;
  ;
  const v85 = stdlib.addressEq(v68, v81);
  stdlib.assert(v85, {
    at: './index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v82), {
    at: './index.rsh:32:17:application',
    fs: ['at ./index.rsh:31:9:application call to [unknown function] (defined at: ./index.rsh:31:13:function exp)'],
    msg: 'got',
    who: 'Bob'
    });
  
  return;
  
  
  
  
  
  
  };
const _ALGO = {
  ABI: {
    impure: [`_reachp_0((uint64,uint64))void`, `_reachp_1((uint64))void`, `_reachp_2((uint64,byte[128]))void`],
    pure: [],
    sigs: [`_reachp_0((uint64,uint64))void`, `_reachp_1((uint64))void`, `_reachp_2((uint64,byte[128]))void`]
    },
  GlobalNumByteSlice: 2,
  GlobalNumUint: 0,
  LocalNumByteSlice: 0,
  LocalNumUint: 0,
  appApproval: `CCAFAAECCCAmAgABADEYQQEwKGRJIls1ASVbNQIpZIIDBBlcRrsEUlb9rATf2SMoNhoAjgMA/ADdAPEAMQA1DjQLIls1DDQLJVs1DYAErNEfwzQMFlA0DRZQsDQMiAEyNA40DRZQIzIGNQI1ASlMVwAoZyg0ARY0AhZQZzEZIhJEiAEYNANAAAqABBUffHU0BFCwI0MjNAESRElXACA1DiEEWzUNNAsXNQyABNUVGRQ0DBZQsDQMiADYNA2IAO00DjQNFlAkMgZC/54kNAESRElXACA1DiEEWzUNNAsiWzUMgASkbdF+NAwWUDQLVwiAULA0DIgAmzQOMQASRDQNNA6IAKQxGYEFEkSIAI8iMgoyCYgAs0L/bIgAc4GgjQY0Bgg1BjYaATULQv8QiABfNhoBNQtC/16IAFQ2GgE1C0L/iSIxNBJEJDE1EkQiMTYSRCIxNxJEiAA1gSivIiJC/wgxGSISREL/ICKyASOyELIHsgiziUiJTAlJNQYyCYgAJ4kJSUH/7kk1BogAH4kjNQOJSSISTDQCEhFEiTQGNAdKD0H/0kL/2rFC/70xFjQAIwhJNQAJRwI4BzIKEkQ4ECMSRDgIEkSJsbIJQv+b`,
  appApprovalMap: {
    0: `2`,
    1: `2`,
    10: `2`,
    100: `48`,
    101: `49`,
    102: `50`,
    103: `51`,
    104: `51`,
    105: `53`,
    106: `53`,
    107: `54`,
    108: `54`,
    109: `55`,
    11: `2`,
    110: `56`,
    111: `57`,
    112: `57`,
    113: `57`,
    114: `58`,
    115: `59`,
    116: `60`,
    117: `60`,
    118: `61`,
    119: `62`,
    12: `2`,
    120: `62`,
    121: `63`,
    122: `64`,
    123: `65`,
    124: `66`,
    125: `66`,
    126: `67`,
    127: `68`,
    128: `69`,
    129: `71`,
    13: `4`,
    130: `71`,
    131: `71`,
    132: `73`,
    133: `73`,
    134: `74`,
    135: `74`,
    136: `74`,
    137: `76`,
    138: `76`,
    139: `76`,
    14: `4`,
    140: `76`,
    141: `76`,
    142: `76`,
    143: `77`,
    144: `77`,
    145: `78`,
    146: `79`,
    147: `81`,
    148: `82`,
    149: `84`,
    15: `5`,
    150: `85`,
    151: `85`,
    152: `86`,
    153: `87`,
    154: `89`,
    155: `90`,
    156: `90`,
    157: `90`,
    158: `91`,
    159: `91`,
    16: `5`,
    160: `92`,
    161: `92`,
    162: `93`,
    163: `94`,
    164: `94`,
    165: `95`,
    166: `95`,
    167: `96`,
    168: `97`,
    169: `97`,
    17: `5`,
    170: `98`,
    171: `98`,
    172: `98`,
    173: `98`,
    174: `98`,
    175: `98`,
    176: `99`,
    177: `99`,
    178: `100`,
    179: `101`,
    18: `6`,
    180: `102`,
    181: `104`,
    182: `104`,
    183: `105`,
    184: `105`,
    185: `105`,
    186: `106`,
    187: `106`,
    188: `107`,
    189: `107`,
    19: `7`,
    190: `107`,
    191: `111`,
    192: `111`,
    193: `112`,
    194: `112`,
    195: `113`,
    196: `114`,
    197: `115`,
    198: `116`,
    199: `116`,
    2: `2`,
    20: `8`,
    200: `117`,
    201: `117`,
    202: `117`,
    203: `119`,
    204: `120`,
    205: `120`,
    206: `121`,
    207: `122`,
    208: `124`,
    209: `125`,
    21: `9`,
    210: `125`,
    211: `125`,
    212: `126`,
    213: `126`,
    214: `127`,
    215: `127`,
    216: `128`,
    217: `129`,
    218: `129`,
    219: `130`,
    22: `10`,
    220: `130`,
    221: `131`,
    222: `132`,
    223: `133`,
    224: `133`,
    225: `134`,
    226: `134`,
    227: `134`,
    228: `134`,
    229: `134`,
    23: `11`,
    230: `134`,
    231: `135`,
    232: `135`,
    233: `136`,
    234: `137`,
    235: `138`,
    236: `138`,
    237: `139`,
    238: `139`,
    239: `139`,
    24: `11`,
    240: `140`,
    241: `141`,
    242: `143`,
    243: `143`,
    244: `144`,
    245: `144`,
    246: `144`,
    247: `145`,
    248: `145`,
    249: `146`,
    25: `12`,
    250: `146`,
    251: `147`,
    252: `148`,
    253: `151`,
    254: `151`,
    255: `153`,
    256: `153`,
    257: `154`,
    258: `154`,
    259: `154`,
    26: `13`,
    260: `156`,
    261: `156`,
    262: `157`,
    263: `157`,
    264: `158`,
    265: `159`,
    266: `161`,
    267: `161`,
    268: `161`,
    269: `163`,
    27: `14`,
    270: `164`,
    271: `164`,
    272: `165`,
    273: `165`,
    274: `166`,
    275: `166`,
    276: `166`,
    277: `167`,
    278: `167`,
    279: `167`,
    28: `14`,
    280: `169`,
    281: `169`,
    282: `169`,
    283: `170`,
    284: `170`,
    285: `170`,
    286: `170`,
    287: `172`,
    288: `172`,
    289: `173`,
    29: `15`,
    290: `174`,
    291: `174`,
    292: `175`,
    293: `175`,
    294: `175`,
    295: `176`,
    296: `176`,
    297: `177`,
    298: `177`,
    299: `177`,
    3: `2`,
    30: `16`,
    300: `179`,
    301: `179`,
    302: `179`,
    303: `180`,
    304: `180`,
    305: `180`,
    306: `181`,
    307: `181`,
    308: `182`,
    309: `182`,
    31: `18`,
    310: `182`,
    311: `184`,
    312: `184`,
    313: `184`,
    314: `185`,
    315: `185`,
    316: `185`,
    317: `186`,
    318: `186`,
    319: `187`,
    32: `18`,
    320: `187`,
    321: `187`,
    322: `189`,
    323: `190`,
    324: `190`,
    325: `191`,
    326: `192`,
    327: `193`,
    328: `194`,
    329: `194`,
    33: `18`,
    330: `195`,
    331: `196`,
    332: `197`,
    333: `198`,
    334: `198`,
    335: `199`,
    336: `200`,
    337: `201`,
    338: `202`,
    339: `202`,
    34: `18`,
    340: `203`,
    341: `204`,
    342: `205`,
    343: `205`,
    344: `205`,
    345: `206`,
    346: `206`,
    347: `207`,
    348: `208`,
    349: `209`,
    35: `18`,
    350: `210`,
    351: `210`,
    352: `210`,
    353: `212`,
    354: `212`,
    355: `213`,
    356: `214`,
    357: `215`,
    358: `217`,
    359: `217`,
    36: `18`,
    360: `217`,
    361: `219`,
    362: `220`,
    363: `220`,
    364: `221`,
    365: `222`,
    366: `222`,
    367: `223`,
    368: `223`,
    369: `224`,
    37: `18`,
    370: `224`,
    371: `225`,
    372: `226`,
    373: `228`,
    374: `229`,
    375: `231`,
    376: `232`,
    377: `233`,
    378: `234`,
    379: `234`,
    38: `18`,
    380: `235`,
    381: `235`,
    382: `236`,
    383: `236`,
    384: `236`,
    385: `237`,
    386: `239`,
    387: `240`,
    388: `241`,
    389: `241`,
    39: `18`,
    390: `241`,
    391: `242`,
    392: `243`,
    393: `243`,
    394: `244`,
    395: `244`,
    396: `244`,
    397: `245`,
    398: `247`,
    399: `248`,
    4: `2`,
    40: `18`,
    400: `248`,
    401: `249`,
    402: `251`,
    403: `252`,
    404: `253`,
    405: `254`,
    406: `255`,
    407: `255`,
    408: `256`,
    409: `257`,
    41: `18`,
    410: `258`,
    411: `259`,
    412: `261`,
    413: `261`,
    414: `262`,
    415: `262`,
    416: `263`,
    417: `264`,
    418: `265`,
    419: `265`,
    42: `18`,
    420: `265`,
    421: `266`,
    422: `266`,
    423: `266`,
    424: `268`,
    425: `269`,
    426: `269`,
    427: `269`,
    428: `272`,
    429: `272`,
    43: `18`,
    430: `273`,
    431: `273`,
    432: `274`,
    433: `275`,
    434: `276`,
    435: `277`,
    436: `277`,
    437: `278`,
    438: `279`,
    439: `279`,
    44: `18`,
    440: `280`,
    441: `280`,
    442: `281`,
    443: `281`,
    444: `282`,
    445: `283`,
    446: `284`,
    447: `284`,
    448: `285`,
    449: `286`,
    45: `18`,
    450: `287`,
    451: `288`,
    452: `288`,
    453: `289`,
    454: `290`,
    455: `291`,
    456: `293`,
    457: `294`,
    458: `294`,
    459: `295`,
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
    65: `26`,
    66: `27`,
    67: `28`,
    68: `29`,
    69: `29`,
    7: `2`,
    70: `30`,
    71: `30`,
    72: `31`,
    73: `32`,
    74: `33`,
    75: `33`,
    76: `34`,
    77: `34`,
    78: `34`,
    79: `34`,
    8: `2`,
    80: `34`,
    81: `34`,
    82: `35`,
    83: `35`,
    84: `36`,
    85: `37`,
    86: `38`,
    87: `38`,
    88: `39`,
    89: `40`,
    9: `2`,
    90: `41`,
    91: `43`,
    92: `43`,
    93: `44`,
    94: `44`,
    95: `44`,
    96: `46`,
    97: `46`,
    98: `47`,
    99: `47`
    },
  appClear: `CA==`,
  appClearMap: {
    },
  companionInfo: null,
  extraPages: 0,
  stateKeys: 1,
  stateSize: 40,
  unsupported: [],
  version: 13,
  warnings: []
  };
const _ETH = {
  ABI: `[{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"internalType":"struct T0","name":"v130","type":"tuple"}],"stateMutability":"payable","type":"constructor"},{"inputs":[{"internalType":"uint256","name":"msg","type":"uint256"}],"name":"ReachError","type":"error"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"indexed":false,"internalType":"struct T0","name":"_a","type":"tuple"}],"name":"_reach_e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"components":[{"internalType":"bytes32","name":"elem0","type":"bytes32"},{"internalType":"bytes32","name":"elem1","type":"bytes32"},{"internalType":"bytes32","name":"elem2","type":"bytes32"},{"internalType":"bytes32","name":"elem3","type":"bytes32"}],"internalType":"struct T3","name":"elem1","type":"tuple"}],"indexed":false,"internalType":"struct T4","name":"_a","type":"tuple"}],"name":"_reach_e2","type":"event"},{"stateMutability":"payable","type":"fallback"},{"inputs":[],"name":"_reachCreationTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentState","outputs":[{"internalType":"uint256","name":"","type":"uint256"},{"internalType":"bytes","name":"","type":"bytes"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T2","name":"v133","type":"tuple"}],"name":"_reachp_1","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"components":[{"internalType":"bytes32","name":"elem0","type":"bytes32"},{"internalType":"bytes32","name":"elem1","type":"bytes32"},{"internalType":"bytes32","name":"elem2","type":"bytes32"},{"internalType":"bytes32","name":"elem3","type":"bytes32"}],"internalType":"struct T3","name":"elem1","type":"tuple"}],"internalType":"struct T4","name":"v136","type":"tuple"}],"name":"_reachp_2","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"stateMutability":"payable","type":"receive"}]`,
  Bytecode: `0x610a7360806001600160401b03601f1938849003601f81018216840190848210848311176102bf57808591604097889485528339810103126102d557835192610047846102da565b805184526020809101519080850191825243600355855190808201828110868211176102bf578752600080925260049560ff8754166102a8577f6de3f97962105ba8e929dd0da178e54f00336c9ea6154699025bad3d4f17547f6060895133815283518582015286518b820152a151801590811561029c575b5015610285573461026e578651926100d7846102da565b8184019383855233905251835260019586835543875587519333838601525188850152878452606084018481108782111761025b578852835195861161024857600254908782811c9216801561023e575b8383101461022b5750601f81116101e4575b508093601f861160011461018157505091839491849394610176575b50501b916000199060031b1c1916176002555b5161077d90816102f68239f35b015192503880610156565b600283528183209493928692918316915b888383106101ca57505050106101b1575b505050811b01600255610169565b015160001960f88460031b161c191690553880806101a3565b858701518855909601959485019487935090810190610192565b60028352818320601f870160051c810191838810610221575b601f0160051c019087905b82811061021657505061013a565b848155018790610208565b90915081906101fd565b634e487b7160e01b845260229052602483fd5b91607f1691610128565b634e487b7160e01b835260419052602482fd5b634e487b7160e01b845260418252602484fd5b865163100960cb60e01b8152600981880152602490fd5b865163100960cb60e01b8152600881880152602490fd5b905060015414386100c0565b875163100960cb60e01b8152600781890152602490fd5b634e487b7160e01b600052604160045260246000fd5b600080fd5b604081019081106001600160401b038211176102bf5760405256fe608060408181526004918236101561001f575b505050361561001d57005b005b600092833560e01c9182631e93b0f1146105b157508163573b85101461033157816357a58e18146100f25750806383230757146100d45763ab53f2c6146100665780610012565b346100d057816003193601126100d0578154610080610653565b91805193849283526020828185015284518093850152815b8381106100b957505060608094508284010152601f80199101168101030190f35b808601820151878201606001528694508101610098565b5080fd5b50346100d057816003193601126100d0576020906001549051908152f35b838360a03660031901126100d05781815161010c81610607565b5280519261011984610638565b80358452608036602319011261032d57815190608082016001600160401b0381118382101761031a5783526024803583526020956044358785015260643585850152608435606085015286810193845260028654036103045761018b61017d610653565b88808251830101910161071a565b9360ff8454166102ee5760c07f3380acc111c840aea9ad8d1987b32a3eb2bf2736fb9b69768ab6c1c26ed72b3791606088519133835285518c8401525180518a8401528b81015182840152898101516080840152015160a0820152a15180159081156102e2575b50156102cd57346102b85782516001600160a01b03929033908416036102a457505083808387829594839551169101519082821561029b575bf11561029157818055816001556102436002546105cd565b80610250575b5051908152f35b601f81116001146102675750816002555b83610249565b6002835283832061028391601f0160051c810190600101610759565b818381208160025555610261565b51903d90823e3d90fd5b506108fc61022b565b601285519163100960cb60e01b8352820152fd5b90601184519163100960cb60e01b8352820152fd5b90601084519163100960cb60e01b8352820152fd5b905060015414876101f2565b855163100960cb60e01b8152600f818601528390fd5b5090600e84519163100960cb60e01b8352820152fd5b634e487b7160e01b855260418252602485fd5b8280fd5b83836020928360031936011261032d5782825161034d81610607565b5281519061035a82610607565b803582526001908185540361059a57610382610374610653565b87808251830101910161071a565b9260ff825416610583577fcf0e8bec53cd91fa87ecf8f6f405ac75914a22acdb92a3553ee5c294fee8159685805133815283518a820152a1518015908115610578575b5015610561578583018051340361054a578451906103e282610638565b86825287820187815294516001600160a01b03169182905251845260028087554384558551808901929092529351818601528481526001600160401b039160608201838111838210176105375786528151928311610524575061044584546105cd565b601f81116104eb575b508690601f831160011461048c579282939183928894610481575b50501b916000199060031b1c19161790555b51908152f35b015192508880610469565b848752878720919083601f198116895b8b888383106104d457505050106104bb575b505050811b01905561047b565b015160001960f88460031b161c191690558680806104ae565b86860151885590960195948501948793500161049c565b61051490858852888820601f850160051c8101918a861061051a575b601f0160051c0190610759565b8761044e565b9091508190610507565b634e487b7160e01b875260419052602486fd5b634e487b7160e01b885260418252602488fd5b845163100960cb60e01b8152600d81840152602490fd5b602490600c85519163100960cb60e01b8352820152fd5b9050825414876103c5565b845163100960cb60e01b8152600b81840152602490fd5b602490600a85519163100960cb60e01b8352820152fd5b8490346100d057816003193601126100d0576020906003548152f35b90600182811c921680156105fd575b60208310146105e757565b634e487b7160e01b600052602260045260246000fd5b91607f16916105dc565b602081019081106001600160401b0382111761062257604052565b634e487b7160e01b600052604160045260246000fd5b604081019081106001600160401b0382111761062257604052565b604051906000600254610665816105cd565b8085526001918083169081156106fb57506001146106a3575b5050829003601f01601f191682016001600160401b0381118382101761062257604052565b600260009081526020935091837f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace5b8385106106e75750505050830101388061067e565b8054888601830152930192849082016106d2565b919250506020925060ff191682850152151560051b830101388061067e565b90816040910312610754576040519061073282610638565b8051906001600160a01b03821682036107545760209183520151602082015290565b600080fd5b818110610764575050565b6000815560010161075956fea164736f6c6343000811000a`,
  BytecodeLen: 2675,
  version: 9,
  views: {
    }
  };
export const _stateSourceMap = {
  1: {
    at: './index.rsh:18:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  2: {
    at: './index.rsh:23:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  3: {
    at: './index.rsh:29:11:after expr stmt semicolon',
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
  "Alice": Alice,
  "Bob": Bob
  };
export const _APIs = {
  };
