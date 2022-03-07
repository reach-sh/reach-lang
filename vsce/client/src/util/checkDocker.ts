import { spawnSync } from 'child_process';

/**
 * @returns {number} `1` if Docker is **not** running.
 */
export default (): number => spawnSync(
	'docker', [ '--version' ]
).status;
