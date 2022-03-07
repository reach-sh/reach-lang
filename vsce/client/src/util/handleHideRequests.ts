import {
	ConfigurationTarget,
	WorkspaceConfiguration
} from 'vscode';

import HIDE_NOTIFICATION_TEXT
	from '../constants/hideNotificationText';

export default (
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
