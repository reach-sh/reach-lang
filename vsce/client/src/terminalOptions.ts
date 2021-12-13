import { TerminalOptions } from 'vscode';

const env: { [key: string]: string } = {
	REACH_IDE: "1"
};

const name = 'Reach IDE';

/**
 * Use these options when creating a terminal so we
 * can do things like add custom environment
 * variables.
 */
export const terminalOptions: TerminalOptions = {
	env,
	name
};

