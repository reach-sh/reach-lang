// Automatically generated with Reach 0.1.13 (56bf3315*)
/* eslint-disable */
export const _version = '0.1.13';
export const _versionHash = '0.1.13 (56bf3315*)';
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
    lct: stdlib.checkedBigNumberify('./examples/overview/index.rsh:17:5:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc1],
    pay: [stdlib.checkedBigNumberify('./examples/overview/index.rsh:17:5:decimal', stdlib.UInt_max, '0'), []],
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
    pay: [stdlib.checkedBigNumberify('./examples/overview/index.rsh:27:5:decimal', stdlib.UInt_max, '0'), []],
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
    at: './examples/overview/index.rsh:27:5:dot',
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
    at: './examples/overview/index.rsh:21:18:application',
    fs: ['at ./examples/overview/index.rsh:20:9:application call to [unknown function] (defined at: ./examples/overview/index.rsh:20:13:function exp)'],
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
    at: './examples/overview/index.rsh:27:5:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
    });
  ;
  stdlib.protect(ctc1, await interact.got(v82), {
    at: './examples/overview/index.rsh:32:17:application',
    fs: ['at ./examples/overview/index.rsh:31:9:application call to [unknown function] (defined at: ./examples/overview/index.rsh:31:13:function exp)'],
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
  Bytecode: `0x608080604052604081610ad9803803809161001a8285610305565b8339810103126102cf576000602060405192610035846102d4565b8051845201516020808401918252436003556040519193919081016001600160401b038111828210176102bb5760405282905260045460ff166102a7577f6de3f97962105ba8e929dd0da178e54f00336c9ea6154699025bad3d4f17547f60606040513381528351602082015285516040820152a151801590811561029b575b5015610287573461027357604051916100cd836102d4565b602083019282845233905251825260018155436001556040519133602084015251604083015260408252610102606083610305565b8151906001600160401b03821161025f57600254600181811c91168015610255575b602082101461024157601f81116101de575b50602090601f831160011461017d57919283610172575b50508160011b916000199060031b1c1916176002555b6040516107b090816103298239f35b01519050388061014d565b90601f1983169360028352818320925b8581106101c6575083600195106101ad575b505050811b01600255610163565b015160001960f88460031b161c1916905538808061019f565b9192602060018192868501518155019401920161018d565b600282527f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace601f840160051c81019160208510610237575b601f0160051c01905b81811061022c5750610136565b82815560010161021f565b9091508190610216565b634e487b7160e01b82526022600452602482fd5b90607f1690610124565b634e487b7160e01b81526041600452602490fd5b63100960cb60e01b81526009600452602490fd5b63100960cb60e01b81526008600452602490fd5b905060015414386100b5565b63100960cb60e01b82526007600452602482fd5b634e487b7160e01b84526041600452602484fd5b600080fd5b604081019081106001600160401b038211176102ef57604052565b634e487b7160e01b600052604160045260246000fd5b601f909101601f19168101906001600160401b038211908210176102ef5760405256fe608080604052600436101561001a575b50361561001857005b005b60003560e01c9081631e93b0f1146105dd57508063573b85101461037957806357a58e18146100f157806383230757146100d35763ab53f2c61461005e573861000f565b346100ce5760003660031901126100ce5760005461007a61068b565b604051918252604060208301528181519182604083015260005b8381106100b65750508160006060809484010152601f80199101168101030190f35b60208282018101516060878401015285935001610094565b600080fd5b346100ce5760003660031901126100ce576020600154604051908152f35b60a03660031901126100ce57600060405161010b816105f8565b5260405161011881610613565b600435815260803660231901126100ce5760405190608082016001600160401b038111838210176103635760405260243582526044356020830152606435604083015260843560608301526020810191825260026000540361034d5761018e61017f61068b565b60208082518301019101610732565b9160ff600454166103375760c07f3380acc111c840aea9ad8d1987b32a3eb2bf2736fb9b69768ab6c1c26ed72b379160606040519133835285516020840152518051604084015260208101518284015260408101516080840152015160a0820152a151801590811561032b575b501561031557346102ff578051336001600160a01b03909116036102e957600080808093602060018060a01b03825116910151908282156102e0575bf1156102d457600080556000600155610251600254610651565b80610262575b602060405160008152f35b601f811160011461027a575060006002555b80610257565b60026000526102bf90601f0160051c600080516020610784833981519152017f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5acf61076c565b60026000526000602081208160025555610274565b6040513d6000823e3d90fd5b506108fc610237565b63100960cb60e01b600052601260045260246000fd5b63100960cb60e01b600052601160045260246000fd5b63100960cb60e01b600052601060045260246000fd5b905060015414826101fb565b63100960cb60e01b600052600f60045260246000fd5b63100960cb60e01b600052600e60045260246000fd5b634e487b7160e01b600052604160045260246000fd5b60203660031901126100ce576000604051610393816105f8565b526040516103a0816105f8565b60043581526001600054036105c7576103ba61017f61068b565b9060ff600454166105b1577fcf0e8bec53cd91fa87ecf8f6f405ac75914a22acdb92a3553ee5c294fee815966040805133815283516020820152a15180159081156105a5575b501561058f57602081019081513403610579576040519161042083610613565b6000835260208301916000835260018060a01b039051168093525181526002600055436001556040519160208301525160408201526040815261046460608261062e565b80516001600160401b0381116103635761047f600254610651565b601f8111610531575b50602091601f82116001146104cd579181926000926104c2575b50508160011b916000199060031b1c191617600255602060405160008152f35b0151905082806104a2565b601f198216926002600052806000209160005b85811061051957508360019510610500575b505050811b01600255610257565b015160001960f88460031b161c191690558280806104f2565b919260206001819286850151815501940192016104e0565b600260005261056990600080516020610784833981519152601f840160051c8101916020851061056f575b601f0160051c019061076c565b82610488565b909150819061055c565b63100960cb60e01b600052600d60045260246000fd5b63100960cb60e01b600052600c60045260246000fd5b90506001541482610400565b63100960cb60e01b600052600b60045260246000fd5b63100960cb60e01b600052600a60045260246000fd5b346100ce5760003660031901126100ce576020906003548152f35b602081019081106001600160401b0382111761036357604052565b604081019081106001600160401b0382111761036357604052565b601f909101601f19168101906001600160401b0382119082101761036357604052565b90600182811c92168015610681575b602083101461066b57565b634e487b7160e01b600052602260045260246000fd5b91607f1691610660565b604051906000826002549161069f83610651565b808352926001811690811561071357506001146106c5575b6106c39250038361062e565b565b506002600090815290916000805160206107848339815191525b8183106106f75750509060206106c3928201016106b7565b60209193508060019154838589010152019101909184926106df565b602092506106c394915060ff191682840152151560051b8201016106b7565b908160409103126100ce576040519061074a82610613565b8051906001600160a01b03821682036100ce5760209183520151602082015290565b818110610777575050565b6000815560010161076c56fe405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5acea164736f6c634300081a000a`,
  BytecodeLen: 2777,
  version: 9,
  views: {
    }
  };
export const _stateSourceMap = {
  1: {
    at: './examples/overview/index.rsh:18:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  2: {
    at: './examples/overview/index.rsh:23:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  3: {
    at: './examples/overview/index.rsh:29:11:after expr stmt semicolon',
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
