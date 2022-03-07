import { spawn } from 'child_process';

export default (
	pathToScript: string,
	versionCompareJson: VersionCompareJson
): Promise<number> => {
	const { dockerC, dockerH, script } = versionCompareJson;
	console.info('dockerC', dockerC);
	console.info('dockerH', dockerH);
	console.info('script', script);

	const argsArray: string[] = [
		'update-ide',
		`--json=${dockerC}`,
		'--rm-json'
	];

	// --script should only be used when "script": true
	if (script) argsArray.push('--script');

	return new Promise((resolve, reject) => {
		const process = spawn(
			pathToScript, argsArray
		);

		process.on('error', reject);
		process.on('exit', resolve);
	});
};
