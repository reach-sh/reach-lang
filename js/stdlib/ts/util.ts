import Timeout from 'await-timeout';
import {
  Signal,
  Lock,
} from './shared_impl';
export const thread = async <T>(f:() => Promise<T>): Promise<T> => await f();

export { Timeout, Signal, Lock };
