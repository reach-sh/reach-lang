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
    lct: stdlib.checkedBigNumberify('./child.rsh:14:5:dot', stdlib.UInt_max, '0'),
    onlyIf: true,
    out_tys: [ctc0],
    pay: [stdlib.checkedBigNumberify('./child.rsh:14:5:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v102], secs: v104, time: v103, didSend: v27, from: v101 } = txn1;
      
      ;
      const v105 = stdlib.checkedBigNumberify('./child.rsh:16:28:decimal', stdlib.UInt_max, '0');
      const v106 = v103;
      
      if (await (async () => {
        const v112 = stdlib.lt(v105, stdlib.checkedBigNumberify('./child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
        
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
  let v105 = stdlib.checkedBigNumberify('./child.rsh:16:28:decimal', stdlib.UInt_max, '0');
  let v106 = v103;
  
  let txn2 = txn1;
  while (await (async () => {
    const v112 = stdlib.lt(v105, stdlib.checkedBigNumberify('./child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
    
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
    const v127 = v123[stdlib.checkedBigNumberify('./child.rsh:19:10:spread', stdlib.UInt_max, '0')];
    ;
    const v130 = stdlib.safeAdd(v105, v102);
    const v131 = stdlib.safeAdd(v130, v127);
    await txn3.getOutput('f', 'v131', ctc0, v131);
    const v138 = stdlib.safeAdd(v105, stdlib.checkedBigNumberify('./child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
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
    at: './child.rsh:1:23:application',
    fs: ['at ./child.rsh:19:20:application call to [unknown function] (defined at: ./child.rsh:19:20:function exp)', 'at ./child.rsh:19:20:application call to [unknown function] (defined at: ./child.rsh:19:20:function exp)'],
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
    pay: [stdlib.checkedBigNumberify('./child.rsh:20:16:decimal', stdlib.UInt_max, '0'), []],
    sim_p: (async (txn1) => {
      const sim_r = { txns: [], mapRefs: [], maps: [] };
      let sim_txn_ctr = stdlib.UInt_max;
      const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };
      
      
      const {data: [v123], secs: v125, time: v124, didSend: v69, from: v122 } = txn1;
      
      sim_r.txns.push({
        kind: 'api',
        who: "f"
        });
      const v127 = v123[stdlib.checkedBigNumberify('./child.rsh:19:10:spread', stdlib.UInt_max, '0')];
      ;
      const v130 = stdlib.safeAdd(v105, v102);
      const v131 = stdlib.safeAdd(v130, v127);
      const v132 = await txn1.getOutput('f', 'v131', ctc0, v131);
      
      const v138 = stdlib.safeAdd(v105, stdlib.checkedBigNumberify('./child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
      const v177 = v138;
      const v179 = stdlib.lt(v138, stdlib.checkedBigNumberify('./child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
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
  const v127 = v123[stdlib.checkedBigNumberify('./child.rsh:19:10:spread', stdlib.UInt_max, '0')];
  ;
  const v130 = stdlib.safeAdd(v105, v102);
  const v131 = stdlib.safeAdd(v130, v127);
  const v132 = await txn1.getOutput('f', 'v131', ctc0, v131);
  if (v69) {
    stdlib.protect(ctc2, await interact.out(v123, v132), {
      at: './child.rsh:19:11:application',
      fs: ['at ./child.rsh:19:11:application call to [unknown function] (defined at: ./child.rsh:19:11:function exp)', 'at ./child.rsh:21:10:application call to "k" (defined at: ./child.rsh:20:23:function exp)', 'at ./child.rsh:20:23:application call to [unknown function] (defined at: ./child.rsh:20:23:function exp)'],
      msg: 'out',
      who: 'f'
      });
    }
  else {
    }
  
  const v138 = stdlib.safeAdd(v105, stdlib.checkedBigNumberify('./child.rsh:22:20:decimal', stdlib.UInt_max, '1'));
  const v177 = v138;
  const v179 = stdlib.lt(v138, stdlib.checkedBigNumberify('./child.rsh:18:16:decimal', stdlib.UInt_max, '2'));
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
  appApproval: `CCAFAAEIAgMmAgABADEYQQEUKGRJIls1ASRbNQIpZIIDBA/jBWkEUlb9rARuqG9INhoAjgMBEgDMAOAANA80EAg0DRcINQuACAAAAAAAAACDNAsWULA0CxY1BDQPIwg1DzIGNQ40DyUMQQDqNBAWNA8WUCEEMgY1AjUBKUxXABBnKDQBFjQCFlBnMRkiEkSIARk0A0AACoAEFR98dTQEULAjQySvNAsWUDULIQQ0ARJESSJbNRAkWzUPNAsiWzUMNAtXCAg1DYAEleaaojQMFlA0DVCwNAyIAN1C/180CyJbNQw0CyRbNRCABKzRH8M0DBZQNBAWULA0DIgAuiI1DzIGNQ5C/16IAJ2BoI0GNAYINQY2GgE1C0L/wogAiTYaATULQv+FIjE0EkQlMTUSRCIxNhJEIjE3EkSIAGqBEK8iIkL/MjEZIhJEQv9KNhoBFzULQv9NMRmBBRJEiABMIjIKMgmIAFpC/ygisgEjshCyB7IIs4lIiUwJSTUGMgmIAEWJCUlB/+5JNQYxFjQAIwhJNQAJRwI4BzIKEkQ4ECMSRDgIEkSJIzUDiTQGNAdKD0H/xEL/zEkiEkw0AhIRRImxsglC/6OxQv+f`,
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
  ABI: `[{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"internalType":"struct T2","name":"v189","type":"tuple"}],"stateMutability":"payable","type":"constructor"},{"inputs":[{"internalType":"uint256","name":"msg","type":"uint256"}],"name":"ReachError","type":"error"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"internalType":"uint256","name":"elem1","type":"uint256"}],"indexed":false,"internalType":"struct T2","name":"_a","type":"tuple"}],"name":"_reach_e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"address","name":"_who","type":"address"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T0","name":"elem1","type":"tuple"}],"indexed":false,"internalType":"struct T1","name":"_a","type":"tuple"}],"name":"_reach_e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"internalType":"uint256","name":"v0","type":"uint256"}],"name":"_reach_oe_v131","type":"event"},{"stateMutability":"payable","type":"fallback"},{"inputs":[],"name":"_reachCreationTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentState","outputs":[{"internalType":"uint256","name":"","type":"uint256"},{"internalType":"bytes","name":"","type":"bytes"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"_reachCurrentTime","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"},{"components":[{"internalType":"uint256","name":"elem0","type":"uint256"}],"internalType":"struct T0","name":"elem1","type":"tuple"}],"internalType":"struct T1","name":"v194","type":"tuple"}],"name":"_reachp_2","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"payable","type":"function"},{"inputs":[{"internalType":"uint256","name":"v184","type":"uint256"}],"name":"f","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"payable","type":"function"},{"stateMutability":"payable","type":"receive"}]`,
  Bytecode: `0x610a4060806001600160401b03601f1938849003601f81018216840190848210848311176102f157808591604097889485528339810103126102ec5783519261004784610307565b80518452602080910151908085019182524360035580865161006881610307565b6000928184809352015260049560ff8754166102d5577f6de3f97962105ba8e929dd0da178e54f00336c9ea6154699025bad3d4f17547f6060895133815283518582015286518b820152a15180159081156102c9575b50156102b2573461029b578651956060870187811087821117610288578852828752818701938385528888019084825251885243905287519361010085610307565b8385528285019784895251809552518752600383556001964388558851948386015251888501528784526060840184811087821117610288578852835195861161027557600254908782811c9216801561026b575b838310146102585750601f8111610211575b508093601f86116001146101ae575050918394918493946101a3575b50501b916000199060031b1c1916176002555b5161071d90816103238239f35b015192503880610183565b600283528183209493928692918316915b888383106101f757505050106101de575b505050811b01600255610196565b015160001960f88460031b161c191690553880806101d0565b8587015188559096019594850194879350908101906101bf565b60028352818320601f870160051c81019183881061024e575b601f0160051c019087905b828110610243575050610167565b848155018790610235565b909150819061022a565b634e487b7160e01b845260229052602483fd5b91607f1691610155565b634e487b7160e01b835260419052602482fd5b634e487b7160e01b845260418252602484fd5b865163100960cb60e01b8152600981880152602490fd5b865163100960cb60e01b8152600881880152602490fd5b905060015414386100be565b875163100960cb60e01b8152600781890152602490fd5b600080fd5b634e487b7160e01b600052604160045260246000fd5b604081019081106001600160401b038211176102f15760405256fe60806040818152600436101561001c575b5050361561001a57005b005b600091823560e01c9081631e93b0f1146101cf5750806346ff359c146101645780638323075714610146578063ab53f2c6146100db5763b3de648b036100105760203660031901126100d7576020916100cf825161007981610225565b8281528481019280845284519061008f82610256565b855161009a81610256565b80835260043590528551916100ae83610225565b8183528783019187516100c081610256565b81815283528352519052610338565b519051908152f35b5080fd5b50346100d757816003193601126100d75781546100f6610271565b91805193849283526020828185015284518093850152815b83811061012f57505060608094508284010152601f80199101168101030190f35b80860182015187820160600152869450810161010e565b50346100d757816003193601126100d7576020906001549051908152f35b5090816003193601126101cc5781519161017d83610225565b818352816020840152805161019181610225565b600435815260203660231901126101c8576020936101c29183516101b481610256565b602435815286820152610338565b51908152f35b8280fd5b80fd5b8390346100d757816003193601126100d7576020906003548152f35b90600182811c9216801561021b575b602083101461020557565b634e487b7160e01b600052602260045260246000fd5b91607f16916101fa565b604081019081106001600160401b0382111761024057604052565b634e487b7160e01b600052604160045260246000fd5b602081019081106001600160401b0382111761024057604052565b604051906000600254610283816101eb565b80855260019180831690811561031957506001146102c1575b5050829003601f01601f191682016001600160401b0381118382101761024057604052565b600260009081526020935091837f405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace5b8385106103055750505050830101388061029c565b8054888601830152930192849082016102f0565b919250506020925060ff191682850152151560051b830101388061029c565b90604080519161034783610256565b600080845260038154036106b25761035d610271565b9383858051810103126100d75783519261037684610225565b8460209687810151865201519186850192835260049760ff89541661069b578651903382528051898301527f263ae805ef0ac75eacb24e0a5ab78e31f247f0b08fe9d5cbf5188647933698b860608a8301938451518b820152a151801590811561068f575b5015610678573461066157907fb1885c0fe128efbc823b7323770c75a9e7e8e72714fc480be1ce86ddb4b4304f88610424819461041b88518b51906106ca565b905151906106ca565b8084528951908152a1519101528351956001600160401b039460608801868111898210176105cf57815283885286880192848452818901958587525189525197600195868a01998a811161064e578a1061064a5789855243905260028099106000146105e25781519361049685610225565b8585528885019186835251809552518152600385554386558151938885015251818401528083526060830190838210878311176105cf575281519485116105bc57506104e286546101eb565b601f8111610583575b508491601f8511600114610524579394508492919083610519575b50501b916000199060031b1c1916179055565b015192503880610506565b86815285812093958591601f198316915b888383106105695750505010610550575b505050811b019055565b015160001960f88460031b161c19169055388080610546565b858701518855909601959485019487935090810190610535565b6105ac90878452868420601f870160051c8101918888106105b2575b601f0160051c01906106f9565b386104eb565b909150819061059f565b634e487b7160e01b835260419052602482fd5b634e487b7160e01b855260418352602485fd5b5050505080809594929350558382556105fb83546101eb565b9182610609575b5050505050565b82601f8694116001146106265750505050555b3880808080610602565b61064091858552601f848620920160051c820191016106f9565b812091555561061c565b8580fd5b634e487b7160e01b875260118552602487fd5b865163100960cb60e01b8152600d818b0152602490fd5b865163100960cb60e01b8152600c818b0152602490fd5b905060015414386103db565b865163100960cb60e01b8152600b818b0152602490fd5b825163100960cb60e01b8152600a6004820152602490fd5b91908201918281116106e35782106106de57565b600080fd5b634e487b7160e01b600052601160045260246000fd5b818110610704575050565b600081556001016106f956fea164736f6c6343000811000a`,
  BytecodeLen: 2624,
  version: 9,
  views: {
    }
  };
export const _stateSourceMap = {
  2: {
    at: './child.rsh:26:11:after expr stmt semicolon',
    fs: [],
    msg: null,
    who: 'Module'
    },
  3: {
    at: './child.rsh:16:27:after expr stmt semicolon',
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
