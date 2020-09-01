// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as child_process from 'child_process';
import { debug } from 'console';

const log = (e: string) => {
	console.log(e);
	vscode.window.showInformationMessage(e);
};

function onCmd(
	err: child_process.ExecException | null,
	stdout: string,
	stderr: string
) {
	// log('cwd: ' + process.cwd());
	// log('root: ' + vscode.workspace.rootPath);
	log(stdout || 'wat? no stdout');
	if (stderr) {
		log('stderr: ' + stderr);
	}
	if (err) {
		log('error: ' + err);
	}
}

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "reach" is now active!');

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	let disposable = vscode.commands.registerCommand('reach.run', () => {
		// The code you place here will be executed every time your command is executed

		// const term = vscode.window.createTerminal();
		const oldCwd = process.cwd();
		const cmd = `(cd ${vscode.workspace.rootPath} && ./reach run)`;
		child_process.exec(cmd, onCmd);

        // constructor(commandLine: string, options?: ShellExecutionOptions);

		// Display a message box to the user
		// vscode.window.showInformationMessage('Reach run functionality coming soon!');
	});

	context.subscriptions.push(disposable);
}

// this method is called when your extension is deactivated
export function deactivate() {}
