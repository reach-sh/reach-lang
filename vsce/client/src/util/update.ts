import { execSync, spawn } from 'child_process';
import {
	ProgressLocation,
	ProgressOptions,
	window
} from 'vscode';

import VERSION_COMPARE_JSON_USING
	from './spawnVersionCompareJson';

const ASYNC_UPDATE_USING = (
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

const UPDATE_SCRIPT_AND_CLI_IMAGE_USING = (
	pathToScript: string
): Buffer => execSync(
	'docker pull "reachsh/reach-cli:latest" ' +
	`&& curl https://docs.reach.sh/reach -o "${
		pathToScript
	}" && chmod +x "${pathToScript}"`
);

export default async (
	pathToScript: string,
	jsonFromVersionCompareJson: VersionCompareJson,
	versionCompareJsonDoesntWork: boolean
) => {
	const cancellable: boolean = false;

	const location: ProgressLocation =
		ProgressLocation.Notification;

	const title: string = "Updating Reach...";

	const progressOptions: ProgressOptions = {
		cancellable, location, title
	};

	// The actual update logic is here!
	window.withProgress(progressOptions, () =>
		new Promise<void>(resolve => setTimeout(
			async () => {
				if (versionCompareJsonDoesntWork) {
					UPDATE_SCRIPT_AND_CLI_IMAGE_USING(
						pathToScript
					);

					console.info(
						'version compare --json ' +
						'should now work.'
					);
					jsonFromVersionCompareJson =
						await VERSION_COMPARE_JSON_USING(
							pathToScript
						);
				}

				if (jsonFromVersionCompareJson) {
					console.info(
						'Update started:',
						new Date().toLocaleTimeString()
					);

					await ASYNC_UPDATE_USING(
						pathToScript,
						jsonFromVersionCompareJson
					);

					console.info(
						'Update finished:',
						new Date().toLocaleTimeString()
					);
				}

				console.info(
					'Done checking for updates',
					new Date().toLocaleTimeString()
				);
				resolve();
			}, 0
		))
	).then(() => window.showInformationMessage(
		'Reach is now up to date.',
		'Close (Esc)'
	));
};
