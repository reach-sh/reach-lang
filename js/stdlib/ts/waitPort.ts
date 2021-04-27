import rawWaitPort from 'wait-port';
import url from 'url';
import { debug, WPArgs, } from './shared';

const protocolPort = {
  'https:': 443,
  'http:': 80,
};

export default async function waitPort(uri: string, givenPort: string | number | undefined = undefined) {
  const { hostname, port, protocol } = url.parse(uri);
  if (!(protocol === 'http:' || protocol === 'https:')) {
    throw Error(`Unsupported protocol ${protocol}`);
  }
  const thePort = port || givenPort;
  const testPort = (typeof thePort === 'string' && parseInt(thePort, 10)) || (typeof thePort === 'number' && thePort) || protocolPort[protocol];
  const args: WPArgs = {
    host: hostname || undefined,
    port: testPort,
    output: 'silent',
    timeout: 1000 * 60 * 1,
  }
  debug('waitPort', {uri, givenPort, thePort, testPort}, args);
  await rawWaitPort(args);
  debug('waitPort complete');
};
