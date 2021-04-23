import ethers, {BigNumber} from 'ethers';

export abstract class TypeDef {
  abstract name: string
  abstract canonicalize(uv: unknown): unknown
}

export abstract class TypeDefs {
  abstract readonly T_Null: A_T_Null
  abstract readonly T_Bool: A_T_Bool
  abstract readonly T_UInt: A_T_UInt
  abstract readonly T_Digest: A_T_Digest
  abstract readonly T_Address: A_T_Address
  abstract readonly T_Token: A_T_Token
  abstract T_Bytes(size: number): A_T_Bytes
  abstract T_Array(td: TypeDef, size: number): A_T_Array
  abstract T_Tuple(tds: TypeDef[]): A_T_Tuple
  abstract T_Struct(namedTds: [string, TypeDef][]): A_T_Struct
  abstract T_Object(tdMap: {[key: string]: TypeDef}): A_T_Object
  abstract T_Data(tdMap: {[key: string]: TypeDef}): A_T_Data
}

export class A_T_Null extends TypeDef {
  name = 'Null'
  canonicalize(uv: unknown): null {
    // Doesn't check with triple eq; we're being lenient here
    if (uv != null) {
      throw Error(`Expected null, but got ${JSON.stringify(uv)}`);
    }
    return null;
  }
}

export class A_T_Bool extends TypeDef {
  name = 'Bool'
  canonicalize(uv: unknown): boolean {
    if (typeof(uv) !== 'boolean') {
      throw Error(`Expected boolean, but got ${JSON.stringify(uv)}`);
    }
    return uv;
  }
}

export abstract class A_T_UInt extends TypeDef {
  abstract readonly width: number
  name = 'UInt'
  canonicalize(uv: unknown): BigNumber {
    try {
      const val = ethers.BigNumber.from(uv);
      return val;
    } catch (e) {
      if (typeof(uv) === 'string') {
        throw Error(`String does not represent a BigNumber. ${JSON.stringify(uv)}`);
      } else {
        throw Error(`Expected BigNumber, number, or string, but got ${JSON.stringify(uv)}`);
      }
    }
  }
}

export class A_T_Bytes extends TypeDef {
  private readonly _name: string
  get name() { return this._name }

  readonly len: number

  constructor(len: number) {
    super();
    this._name = `Bytes(${len})`;
    this.len = len;
  }

  canonicalize(uv: unknown): string {
    const {len} = this;
    if (typeof(uv) !== 'string') {
      throw Error(`Bytes expected string, but got ${JSON.stringify(uv)}`);
    }
    const checkLen = (label: string, alen: number, fill: string): string => {
      if ( uv.length > alen ) {
        throw Error(`Bytes(${len}) must be a ${label}string less than or equal to ${alen}, but given ${label}string of length ${uv.length}`);
      }
      return uv.padEnd(alen, fill);
    };
    if ( uv.slice(0,2) === '0x' ) {
      return checkLen('hex ', len*2+2, '0');
    } else {
      return checkLen('', len, '\0');
    }
  }
}

export class A_T_Digest extends TypeDef {
  name = 'Digest'

  // TODO: check digest length, or something similar?
  // That's probably best left to connector-specific code.
  canonicalize(uv: unknown): string {
    if (typeof uv !== 'string') {
      throw Error(`${JSON.stringify(uv)} is not a valid digest`);
    }
    return uv;
  }
}

abstract class StringyTypeDef extends TypeDef {
  canonicalize(uv: unknown): string {
    if (typeof uv !== 'string') {
      throw Error(`${this.name} must be a string, but got: ${JSON.stringify(uv)}`);
    }
    return uv;
  }
}

export class A_T_Address extends StringyTypeDef {
  name = 'Address'
}

export class A_T_Token extends StringyTypeDef {
  name = 'Token'
}

export class A_T_Array extends TypeDef {
  private readonly _name: string
  get name() { return this._name }

  readonly td: TypeDef
  readonly size: number

  constructor(td: TypeDef, size: number) {
    super();
    this._name = `Array(${td.name}, ${size})`
    this.td = td;
    this.size = size;
  }

  canonicalize(args: unknown): unknown[] {
    const {size, td} = this;
    if (!Array.isArray(args)) {
      throw Error(`Expected an Array, but got ${JSON.stringify(args)}`);
    }
    if (size !== args.length) {
      throw Error(`Expected array of length ${size}, but got ${args.length}`);
    }
    const val = args.map((arg) => td.canonicalize(arg));
    return val;
  }
}

export class A_T_Tuple extends TypeDef {
  private readonly _name: string
  get name() { return this._name }

  readonly ctcs: TypeDef[]

  constructor(ctcs: TypeDef[]) {
    super();
    this.ctcs = ctcs;
    this._name = `Tuple(${ctcs.map((ctc) => ` ${ctc.name} `)})`;
  }

  canonicalize(args: unknown): unknown[] {
    const {ctcs} = this;
    if (!Array.isArray(args)) {
      throw Error(`Expected a Tuple, but got ${JSON.stringify(args)}`);
    }
    if (ctcs.length != args.length) {
      throw Error(`Expected tuple of size ${ctcs.length}, but got ${args.length}`);
    }
    const val = args.map((arg, i) => ctcs[i].canonicalize(arg));
    return val;
  }
}

export class A_T_Struct extends TypeDef {
  private readonly _name: string
  get name() { return this._name }

  readonly namedTds: [string, TypeDef][]

  constructor(namedTds: [string, TypeDef][]) {
    super();
    this.namedTds = namedTds;
    this._name = `Struct([${namedTds.map(([k, td]) => ` [${k}, ${td.name}] `)}])`;
  }

  canonicalize(arg: unknown): {[key: string]: unknown} {
    const {namedTds} = this;
    const obj: {
      [key: string]: unknown
    } = {};
    namedTds.forEach(([k, td], i) => {
      // TODO: throw error if  `arg` is obj but doesn't have key `k`
      obj[k] = td.canonicalize(Array.isArray(arg) ? arg[i] : (arg as {[k: string]: unknown})[k]);
    });
    return obj;
  }
}

export class A_T_Object extends TypeDef {
  private readonly _name: string
  get name() { return this._name }

  readonly tdMap: {[key: string]: TypeDef}

  constructor(tdMap: {[key: string]: TypeDef}) {
    super();
    this._name = `Object(${Object.keys(tdMap).map((k) => ` ${k}: ${tdMap[k].name} `)})`;
    this.tdMap = tdMap;
  }

  canonicalize(uvo: unknown): {[key: string]: unknown} {
    const {tdMap} = this;
    if (typeof(uvo) !== 'object') {
      throw Error(`Expected object, but got ${JSON.stringify(uvo)}`);
    }
    // XXX not sure what else TypeScript wants me to check to assert this is the case
    const vo = uvo as {[key: string]: unknown};
    const obj: {
      [key: string]: unknown
    } = {};
    for (const prop in tdMap) {
      // This is dumb but it's how ESLint says to do it
      // https://eslint.org/docs/rules/no-prototype-builtins
      if (!{}.hasOwnProperty.call(vo, prop)) {
        throw Error(`Expected prop ${prop}, but didn't found it in ${Object.keys(vo)}`);
      }
      obj[prop] = tdMap[prop].canonicalize(vo[prop]);
    }
    return obj;
  }
}

export class A_T_Data extends TypeDef {
  private readonly _name: string
  get name() { return this._name }

  readonly tdMap: {[key: string]: TypeDef}

  constructor(tdMap: {[key: string]: TypeDef}) {
    super();
    this._name = `Data(${Object.keys(tdMap).map((k) => ` ${k}: ${tdMap[k].name} `)})`;
    this.tdMap = tdMap;
  }

  canonicalize(io: unknown): [string, unknown] {
    const {tdMap} = this;
    if (!(Array.isArray(io) && io.length == 2 && typeof io[0] == 'string')) {
      throw Error(`Expected an array of length two to represent a data instance, but got ${JSON.stringify(io)}`);
    }
    const vn = io[0];
    if (!{}.hasOwnProperty.call(tdMap, vn)) {
      throw Error(`Expected a variant in ${Object.keys(tdMap)}, but got ${vn}`);
    }
    return [vn, tdMap[vn].canonicalize(io[1])];
  }
}
