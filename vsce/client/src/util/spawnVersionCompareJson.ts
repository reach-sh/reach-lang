import { OutputChannel } from 'vscode';
import { spawn } from 'child_process';

export default (
	pathToScript: string,
	outputChannel: OutputChannel
): Promise<VersionCompareJson> => {
	outputChannel.appendLine(
		'Spawned version-compare --json at ' +
		new Date().toLocaleTimeString()
	);

	// https://stackoverflow.com/a/53204227
	return new Promise((resolve, reject) => {
		const process = spawn(
			pathToScript, ['version-compare', '--json']
		);

		process.on('exit', code => {
			outputChannel.appendLine(
				'version-compare --json finished' + ' at '
				+ new Date().toLocaleTimeString()
			);

			const { stdout } = process;

			console.info(
				'version-compare --json\'s process', process
			);

			outputChannel.appendLine(
				'version-compare --json exited with ' +
				`status code ${code}.`
			);

			// "exit code 60 means either the script or
			// the Docker bits are out of date (or both)"
			if (code === 60) {
				const json = JSON.parse(
					stdout.read(stdout.readableLength)
				);
				outputChannel.appendLine(
					'version-compare --json output:'
				);
				outputChannel.appendLine(
					JSON.stringify(json)
				);
				return resolve(json);
			} else if (code === 0) {
				outputChannel.appendLine(
					'You should be up to date. ' +
					new Date().toLocaleTimeString()
				);
			} else {
				outputChannel.appendLine(
					'Error: version-compare --json gave ' +
					`an unexpected exit code: ${code}`
				);
				outputChannel.appendLine(
					'version-compare --json stdout:'
				);
				outputChannel.appendLine(
					process.stdout.read(
						process.stdout.readableLength
					)
				);
				outputChannel.appendLine(
					'version-compare --json stderr:'
				);
				outputChannel.appendLine(
					process.stderr.read(
						process.stderr.readableLength
					)
				);
				reject(null);
			}

			resolve(null);
		});

		process.on('error', reject);
	});
};
