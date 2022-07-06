import fetch from 'node-fetch';


// Types

type JWT = string;

type ID = string;

type CreateAccount =
  { ca_firstName   : string
  , ca_lastName    : string
  , ca_username    : string
  , ca_email       : string
  , ca_password    : string
  , ca_phoneNumber : string
  , ca_mfaSms?     : string
  , ca_mfaEmail?   : string
  , ca_mfaTotp?    : boolean }

type Login =
  { lf_username : string
  , lf_password : string
  }

type MfaInfo =
  { mi_userId    : ID
  , mi_mfaSms?   : string
  , mi_mfaEmail? : string
  , mi_mfaTotp?  : boolean
  }

type UpdateAccount =
  { ua_firstName   : string
  , ua_lastName    : string
  , ua_phoneNumber : string
  , ua_mfaSms?     : string
  , ua_mfaEmail?   : string
  , ua_mfaTotp?    : boolean }

type UpdateValueFormInfo =
  { uv_newVal : string
  }

type VerifyMfa =
  { vm_id?      : ID
  , vm_verCode? : string }

type User =
  { userId      : ID
  , firstName   : string
  , lastName    : string
  , email       : string
  , phoneNumber : string
  , mfaSms?     : string
  , mfaEmail?   : string
  , mfaTotp?    : boolean }


const baseUrl = `http://127.0.0.1:8080`;

type QueryParam = [string, string];

const mkEndpoint = (endpoint: string, queryParams: QueryParam[] = []) => {
  const p = queryParams
              .map(([f, v]: QueryParam) => `${f}=${v}`)
              .join('&');
  const ps = (queryParams.length > 0) ? `?${p}` : ``;
  return `${baseUrl}/${endpoint}${ps}`;
}


type Env = {
  jwt: JWT | undefined,
  mfaInfo: MfaInfo | undefined,
  user: User | undefined,
}

type Opts = {
  useEnv: boolean,
  debug : boolean
}

type FetchOpts = { auth: JWT } | undefined

export class ReachID {

  opts: Opts = {
    useEnv: false,
    debug : false,
  }

  env: Env = {
    jwt: undefined,
    mfaInfo: undefined,
    user: undefined
  };

  constructor(opts: Opts) {
    this.opts = opts;
  }


  // Helpers

  async _doFetch(method: string, url: string, msg: Object, opts: FetchOpts) {
    const req = ({
      method,
      // @ts-ignore
      credentials: 'same-origin',
      headers: {
        'Accept': 'application/json',
        'Content-type': 'application/json',
        ...(opts?.auth ? { 'Authorization': `Bearer ${opts.auth}` } : {})
      },
      ...(method != `GET` ? { body: JSON.stringify(msg) } : {})
    });

    if (this.opts.debug) { console.debug(`Request:`, url, req); }

    const response = await fetch(url, req) as Response;
    if (this.opts.debug) { console.debug(`Response:`, response); }

    const json = await response.json();
    if (this.opts.debug) { console.debug(`Response Body:`, json); }

    const mAuth = response.headers.get('authorization');
    if (mAuth) {
      this.env.jwt = mAuth!;
    }

    return json;
  }

  _get(url: string, msg: Object, opts: FetchOpts = undefined) {
    return this._doFetch(`GET`, url, msg, opts);
  }

  _post(url: string, msg: Object, opts: FetchOpts = undefined) {
    return this._doFetch(`POST`, url, msg, opts);
  }

  _findUserId(opts: { orDie: boolean }) {
    if (this.opts.useEnv) {
      if (this.env.mfaInfo?.mi_userId) {
        return this.env.mfaInfo?.mi_userId;
      }
      if (this.env.user?.userId) {
        return this.env.user?.userId;
      }
      // ...
    }
    if (opts.orDie) {
      throw Error(`No id provided and none was found in the environment.`);
    } else {
      return undefined;
    }
  }

  _ensureUserId(val: any, idKey: string) {
    if (val[idKey] == undefined) {
      val[idKey] = this._findUserId({ orDie: true });
    }
    return val;
  }

  _mkAuth(jwt: JWT | undefined) {
    let auth = undefined;
    if (this.opts.useEnv) {
      auth = this.env.jwt || jwt;
    } else {
      auth = jwt;
    }
    if (auth == undefined) {
      throw Error(`No JWT provided and none was found in the environment.`);
    }
    return { auth };
  }


  // Methods

  async login(msg: Login): Promise<MfaInfo> {
    const url = mkEndpoint(`login`);
    const mfaInfo = await this._post(url, msg) as MfaInfo;
    if (this.opts.useEnv) {
      this.env.mfaInfo = mfaInfo;
    }
    return mfaInfo;
  }

  async logout(jwt: JWT | undefined = undefined): Promise<void> {
    const url = mkEndpoint(`logout`);
    await this._post(url, {}, this._mkAuth(jwt));
    if (this.opts.useEnv) {
      this.env.jwt = undefined;
    }
  }

  async verifyMfa(info: VerifyMfa): Promise<User> {
    const msg = this._ensureUserId(info, `vm_id`);
    const url = mkEndpoint(`verifyMfa`);
    const user = await this._post(url, msg);
    return user;
  }

  async createAccount(msg: CreateAccount): Promise<MfaInfo> {
    const url = mkEndpoint(`createAccount`);
    const mfaInfo = await this._post(url, msg);
    return mfaInfo;
  }

  async sendVerificationCode(userId: ID | undefined = undefined): Promise<MfaInfo> {
    const id = userId || this._findUserId({ orDie: true })!;
    const url = mkEndpoint(`sendVerificationCode`, [ [`id`, id] ]);
    const mfaInfo = await this._post(url, {});
    return mfaInfo;
  }

  async getAccountInfo(jwt: JWT | undefined = undefined): Promise<User> {
    const url = mkEndpoint(`getAccountInfo`);
    const user = await this._get(url, {}, this._mkAuth(jwt));
    if (this.opts.useEnv) {
      this.env.user = user;
    }
    return user;
  };

  async updateAccount(msg: UpdateAccount, jwt: JWT | undefined = undefined): Promise<User> {
    const url = mkEndpoint(`updateAccount`);
    const user = await this._post(url, msg, this._mkAuth(jwt));
    if (this.opts.useEnv) {
      this.env.user = user;
    }
    return user;
  };

  async updateEmail(newEmailAddress: string, jwt: JWT | undefined = undefined): Promise<void> {
    const url = mkEndpoint(`updateEmail`);
    const msg : UpdateValueFormInfo = { uv_newVal: newEmailAddress };
    await this._post(url, msg, this._mkAuth(jwt));
  };

  async updatePassword(newPassword: string, jwt: JWT | undefined = undefined): Promise<MfaInfo> {
    const url = mkEndpoint(`updatePassword`);
    const msg : UpdateValueFormInfo = { uv_newVal: newPassword };
    const mfaInfo = await this._post(url, msg, this._mkAuth(jwt));
    if (this.opts.useEnv) {
      this.env.mfaInfo = mfaInfo;
    }
    return mfaInfo;
  };

}
