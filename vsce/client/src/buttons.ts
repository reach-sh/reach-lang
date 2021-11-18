import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';

let shownButtons: vscode.StatusBarItem[] = [];

function createButtons(buttons: string[][]) {
	buttons.forEach((button: string[]) => {
		const buttonBarItem = vscode.window.createStatusBarItem(1, 0);
		buttonBarItem.text = button[0];
		buttonBarItem.command = button[1];
		buttonBarItem.show();
		shownButtons.push(buttonBarItem);
	});
}

function removeAllButtons() {
	return new Promise<void>((resolve, reject) => {
		shownButtons.forEach(button => button.hide());
		shownButtons = [];
		resolve();
	});
}

function showButtons() {
	createButtons([
		['Reach Compile', 'reach.compile'],
		['Reach Run', 'reach.run'],
	]);
}

export const initButtons = (context: ExtensionContext) => {
	showButtons();
};
