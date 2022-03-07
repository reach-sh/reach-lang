import {
	ProgressLocation,
	ProgressOptions,
	window
} from 'vscode';

import ASYNC_UPDATE_USING from './spawnUpdateIde';
import UPDATE_SCRIPT_AND_CLI_IMAGE_USING
	from './updateScriptAndCliImage';
import VERSION_COMPARE_JSON_USING
	from './spawnVersionCompareJson';

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
