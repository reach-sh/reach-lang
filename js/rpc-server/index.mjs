import { serveRpc } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

serveRpc(backend);
