import { execSync } from 'child_process';

export default (pathToScript: string): Buffer => execSync(
	'docker pull "reachsh/reach-cli:latest" ' +
	`&& curl https://docs.reach.sh/reach -o "${
		pathToScript
	}" && chmod +x "${pathToScript}"`
);
