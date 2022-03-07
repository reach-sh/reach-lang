import { spawn } from 'child_process';

/**
 * asynchronously checks whether a user can do
 * `version-compare --json -h >/dev/null 2>&1`.
 * @returns {Promise<number>} a `Promise` that `resolve`s
 * to `true` if `version compare --json` **doesn't** work
 * for whatever reason.
 */

export default (
	pathToScript: string
): Promise<boolean> => new Promise((resolve, reject) => {
	// https://stackoverflow.com/a/53204227
	const process = spawn(
		pathToScript, [
			'version-compare',
			'--json',
			'-h',
			'>/dev/null',
			'2>&1'
		]
	);

	process.on('error', reject);
	process.on('exit', code => resolve(!!code));
});
