import { serveRpc } from '@reach-sh/stdlib/rpc_server.mjs';
import * as backend from './build/index.main.mjs';

serveRpc(backend);
