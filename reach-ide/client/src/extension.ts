/* --------------------------------------------------------------------------------------------
 * Copyright for portions from https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample
 * are held by (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 *
 * Copyright (c) 2020 Eric Lau. All rights reserved.
 * Licensed under the Eclipse Public License v2.0
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, commands, window, env, ViewColumn, Uri, WorkspaceFolder } from 'vscode';
import { exec } from 'child_process';
import { initButtons } from './buttons';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient';

let client: LanguageClient;

var terminal;

const fs = require('fs')
const url = require('url')

var rootFolder: string;

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	let serverModule = context.asAbsolutePath(
		path.join('server', 'out', 'server.js')
	);
	// The debug options for the server
	// --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
	let debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	let serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
			options: debugOptions
		}
	};

	terminal = window.createTerminal({ name: "Reach IDE" });
	registerCommands(context);


	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for Reach .rsh documents
		documentSelector: [
			{
			  pattern: '**/*.rsh',
			  scheme: 'file'
			}
		],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/*.rsh')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'reachide',
		'Reach IDE',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();

	initButtons(context);

	// Inject association for .rsh file type
	if (workspace.workspaceFolders !== undefined) {
		rootFolder = url.fileURLToPath( workspace.workspaceFolders[0].uri.toString() );
	};
	associateRshFiles();
}

function registerCommands(context: ExtensionContext) {
	const disposable = commands.registerCommand('reach.compile', () => {
		terminal.show();
		terminal.sendText("./reach compile");
	});
	context.subscriptions.push(disposable);

	const disposable2 = commands.registerCommand('reach.run', () => {
		terminal.show();
		terminal.sendText("./reach run");
	});
	context.subscriptions.push(disposable2);

	const disposable3 = commands.registerCommand('reach.upgrade', () => {
		terminal.show();
		terminal.sendText("./reach upgrade");
	});
	context.subscriptions.push(disposable3);

	const disposable6 = commands.registerCommand('reach.update', () => {
		terminal.show();
		terminal.sendText("./reach update");
	});
	context.subscriptions.push(disposable6);

	// Launch docs website
	const disposable7 = commands.registerCommand('reach.docs', () => {
		env.openExternal(Uri.parse('https://docs.reach.sh/doc-index.html'));
	});
	context.subscriptions.push(disposable7);

}

function associateRshFiles() {
	exec(`mkdir -p ${rootFolder}${path.sep}.vscode`, (error: { message: any; }, stdout: any, stderr: any) => {
		if (error) {
			console.error(`Could not create .vscode directory: ${error.message}`);
			return;
		}
		if (stderr) {
			console.error(`Could not create .vscode directory: ${stderr}`);
			return;
		}
		injectRshFileAssocation();
	});
}

function injectRshFileAssocation() {
	var settingsFile:string = `${rootFolder}${path.sep}.vscode/settings.json`;

	fs.readFile(settingsFile, function (err: any, content: string) {
		var parseJson;
		try {
			parseJson = JSON.parse(content);
		} catch {
			parseJson = {}
		}
		var fileAssoc = parseJson["files.associations"]
		if (fileAssoc == undefined) {
			parseJson["files.associations"] = { "*.rsh": "javascript" }
		} else {
			parseJson["files.associations"]["*.rsh"] = "javascript";
		}
		fs.writeFile(settingsFile, JSON.stringify(parseJson), function (err: any) {
			if (err) {
				console.error(`Could not create .vscode/settings.json: ${err}`);
				return;
			}
		})
	})
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
