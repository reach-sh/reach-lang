import { spawn } from 'child_process';

export default (
	pathToScript: string
): Promise<VersionCompareJson> => {
	// Track times to find performance bottlenecks.
	console.info(
		'Spawned version-compare --json at',
		new Date().toLocaleTimeString()
	);

	// https://stackoverflow.com/a/53204227
	return new Promise((resolve, reject) => {
		const process = spawn(
			pathToScript, ['version-compare', '--json']
		);

		process.on('exit', code => {
			console.info(
				'version-compare --json finished...',
				process,
				new Date().toLocaleTimeString()
			);

			const { stdout } = process;

			console.info(
				'version-compare --json exited',
				'with status code',
				code
			);

			// "exit code 60 means either the script or
			// the Docker bits are out of date (or both)"
			if (code === 60) {
				const json = JSON.parse(
					stdout.read(stdout.readableLength)
				);
				console.info('json!', json);
				return resolve(json);
			}

			if (code) {
				console.error('Unexpected exit code:', code);
				reject(null);
			}

			resolve(null);
		});

		process.on('error', reject);
	});
};
