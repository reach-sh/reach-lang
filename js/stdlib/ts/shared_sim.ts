import { ethers } from 'ethers';
import {
  Stdlib_SimStuff,
} from './interfaces';
import {
  ISimRes,
  ISimRemote,
} from './shared_impl';
import {
  AnyBackendTy,
  MaybeRep,
  mapRef,
  mapSet,
  LinearMap,
  copyMap,
} from './shared_backend';
type BigNumber = ethers.BigNumber;

export const defineSimStuff = <Token, ContractInfo, ConnectorTy extends AnyBackendTy>(): Stdlib_SimStuff<Token, ContractInfo, ConnectorTy> => {
  type SimRes = ISimRes<Token, ContractInfo, ConnectorTy>;

  const simMapDupe = <K, A>(sim_r:SimRes, mapi:number, mapo:LinearMap<K, A, ConnectorTy>): void => {
    sim_r.maps[mapi] = copyMap(mapo.ref);
  };

  const simMapRef = async <K, A>(sim_r:SimRes, mapi:number, kt:ConnectorTy, k:K, vt:ConnectorTy): Promise<MaybeRep<A>> => {
    sim_r.mapRefs.push({ kind: 'ref', mapi, kt, k, vt });
    return await mapRef(sim_r.maps[mapi], kt, k, vt);
  };

  const simMapSet = async <K, A>(sim_r:SimRes, mapi:number, kt:ConnectorTy, k:K, vt:ConnectorTy, v:A|undefined): Promise<void> => {
    const kind = v ? 'set' : 'del';
    sim_r.mapRefs.push({ kind, mapi, kt, k, vt });
    return await mapSet(sim_r.maps[mapi], kt, k, vt, v);
  };

  const simTokenNew = <A>(sim_r:SimRes, n:any, s:any, u:any, m:any, p:BigNumber, d:BigNumber|undefined, ctr:A): A => {
    sim_r.txns.push({kind: 'tokenNew', n, s, u, m, p, d });
    // XXX This is a hack... it is assumed that `ctr` is unique across tokens in a simulation block
    return ctr;
  };

  const simContractNew = <A>(sim_r:SimRes, cns:any, remote:ISimRemote<Token, ContractInfo>, ctr:A): A => {
    sim_r.txns.push({kind: 'contractNew', cns, remote });
    // XXX This is a hack... it is assumed that `ctr` is unique across tokens in a simulation block
    return ctr;
  };

  const simTokenBurn = (sim_r:SimRes, tok:Token, amt:BigNumber): void => {
    sim_r.txns.push({kind: 'tokenBurn', tok, amt});
  };

  const simTokenDestroy = (sim_r:SimRes, tok:Token): void => {
    sim_r.txns.push({kind: 'tokenDestroy', tok});
  };

  const simTokenAccepted_ = (sim_r:SimRes, addr:string, tok:Token): void => {
    sim_r.txns.push({kind: 'tokenAccepted', addr, tok});
  };

  return { simMapDupe, simTokenAccepted_, simTokenDestroy, simTokenBurn, simContractNew, simTokenNew, simMapSet, simMapRef };
};


