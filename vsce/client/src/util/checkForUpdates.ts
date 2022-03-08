import { spawnSync } from 'child_process';
import {
	ConfigurationTarget,
	window,
	workspace,
	WorkspaceConfiguration
} from 'vscode';

import CHECK_VERSION_COMPARE_JSON_USING
	from './checkVersionCompareJson';
import UPDATE_USING from './update';
import VERSION_COMPARE_JSON_USING
	from './spawnVersionCompareJson';

const OUTPUT_CHANNEL = window.createOutputChannel(
	'Reach update status'
);

const HIDE_NOTIFICATION_TEXT = 'Hide Reach update messages';

/**
 * @returns {number} `1` if Docker is **not** running.
 */
 const DOCKER_IS_NOT_RUNNING = (): number => spawnSync(
	'docker', [ '--version' ]
).status;

const HANDLE_HIDE_REQUEST = (
	response: string,
	thisExtensionsSettings: WorkspaceConfiguration
): string => {
	if (response === HIDE_NOTIFICATION_TEXT)
		thisExtensionsSettings.update(
			'showUpdateNotifications',
			false,
			ConfigurationTarget.Global
		);

	return response;
};

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

	OUTPUT_CHANNEL.appendLine(
		'Starting update check at ' +
		new Date().toLocaleTimeString()
	);

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

	OUTPUT_CHANNEL.appendLine(
		'Checking version-compare --json -h >/dev/null 2>&1 '
		+ new Date().toLocaleTimeString()
	);

	CHECK_VERSION_COMPARE_JSON_USING(pathToScript).then(
		async versionCompareJsonDoesntWork => {
			const str = versionCompareJsonDoesntWork ?
				' doesn\'t work. ' : ' works. ';

			OUTPUT_CHANNEL.appendLine(
				'version-compare --json -h >/dev/null 2>&1'
				+ str + new Date().toLocaleTimeString()
			);

			let jsonFromVersCompJson: VersionCompareJson = null;

			const versionCompareJsonWorks =
				!versionCompareJsonDoesntWork;

			if (versionCompareJsonWorks) {
				try {
					jsonFromVersCompJson =
						await VERSION_COMPARE_JSON_USING(
							pathToScript
						);
				} catch (error) {
					OUTPUT_CHANNEL.appendLine(
						'Checking for updates failed.'
					);
					OUTPUT_CHANNEL.append(
						'version-compare --json exited '
					);
					OUTPUT_CHANNEL.append(
						'with an unexpected status code. '
					);
					OUTPUT_CHANNEL.appendLine(
						new Date().toLocaleTimeString()
					);
				}
			}

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
