import { rpc_server } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const { serveRpc } = rpc_server;
serveRpc(backend);
