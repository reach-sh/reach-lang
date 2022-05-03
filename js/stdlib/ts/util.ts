import Timeout from 'await-timeout';
import {
  Signal,
  Lock,
} from './shared_impl';
export const thread = async (f) => await f();

export { Timeout, Signal, Lock };
