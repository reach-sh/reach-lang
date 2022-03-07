import { window, workspace } from 'vscode';

import CHECK_VERSION_COMPARE_JSON_USING
	from './checkVersionCompareJson';
import DOCKER_IS_NOT_RUNNING
	from './checkDocker';
import HANDLE_HIDE_REQUEST
	from './handleHideRequests';
import HIDE_NOTIFICATION_TEXT
	from '../constants/hideNotificationText';
import UPDATE_USING from './update';
import VERSION_COMPARE_JSON_USING
	from './spawnVersionCompareJson';

export default async (
	pathToScript: string
): Promise<string | void> => {
	const thisExtensionsSettings =
		workspace.getConfiguration('reachide');

	console.debug(thisExtensionsSettings);

	const showUpdateNotifications =
		thisExtensionsSettings.get(
			'showUpdateNotifications'
		) as boolean;

	console.debug(
		'showUpdateNotifications is',
		showUpdateNotifications
	);

	// Respect the user's preferences.
	if (showUpdateNotifications === false) return;

	console.info('Starting update check');

	if (DOCKER_IS_NOT_RUNNING())
		return await window.showInformationMessage(
			'Failed to check for Reach updates: ' +
			'Docker doesn\'t appear to be running. ' +
			'Start it, and then reload this window if ' +
			'you would like to check for Reach updates.',
			HIDE_NOTIFICATION_TEXT, 'Close (Esc)'
		).then(response => HANDLE_HIDE_REQUEST(
			response,
			thisExtensionsSettings
		));

	console.info(
		'About to check whether ' +
		'version-compare --json -h >/dev/null 2>&1 works',
		new Date().toLocaleTimeString()
	);

	CHECK_VERSION_COMPARE_JSON_USING(pathToScript).then(
		async versionCompareJsonDoesntWork => {
			console.info(
				'Finished checking whether ' +
				'version-compare --json -h >/dev/null ' +
				'2>&1 works' +
				`\nIt does${
					versionCompareJsonDoesntWork ?
						'n\'t' : ''
				}!`,
				new Date().toLocaleTimeString()
			);

			let jsonFromVersCompJson: VersionCompareJson = null;

			const versionCompareJsonWorks =
				!versionCompareJsonDoesntWork;

			if (versionCompareJsonWorks)
				jsonFromVersCompJson =
					await VERSION_COMPARE_JSON_USING(
						pathToScript
					);

			let response: string = null;

			if (
				versionCompareJsonDoesntWork ||
				jsonFromVersCompJson
			) {
				response = await window.showInformationMessage(
					'Updates for Reach are available;' +
					'\nwould you like to update now?\n' +
					`This could overwrite ${
						pathToScript
					}`,
					'Yes',
					'No (Esc)',
					HIDE_NOTIFICATION_TEXT
				).then(response => HANDLE_HIDE_REQUEST(
					response,
					thisExtensionsSettings
				));
			}

			if (response === 'Yes')
				UPDATE_USING(
					pathToScript,
					jsonFromVersCompJson,
					versionCompareJsonDoesntWork
				);
		});
};
