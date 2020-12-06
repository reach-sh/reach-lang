import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';

let shownButtons = [];

function createButtons(buttons) {
	buttons.forEach(button => {
		const buttonBarItem = vscode.window.createStatusBarItem(1, 0);
		buttonBarItem.text = button[0];
		buttonBarItem.command = buttons[1];
		buttonBarItem.show();
		shownButtons.push(buttonBarItem);
	});
}

function removeAllButtons() {
	return new Promise((resolve, reject) => {
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
