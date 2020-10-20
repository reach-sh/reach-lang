/* --------------------------------------------------------------------------------------------
 * Copyright for portions from https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample 
 * are held by (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * 
 * Copyright (c) 2020 Eric Lau. All rights reserved. 
 * Licensed under the Eclipse Public License v2.0
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, commands, window } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient';

let client: LanguageClient;

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
	
	/* this doesn't work
	// settings.json configuration
	const config = workspace.getConfiguration('settings');
	config.update("files.associations", { "*.rsh" : "javascript" }, false);
	*/


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

}

function registerCommands(context: ExtensionContext) {
	const disposable = commands.registerCommand('extension.reach.compile', () => {
		// The code you place here will be executed every time your command is executed

		// Compile Reach program
		window.showInformationMessage('Reach compilation successful!');
	});
	context.subscriptions.push(disposable);

	const disposable2 = commands.registerCommand('extension.reach.run', () => {
		// The code you place here will be executed every time your command is executed

		// Compile Reach program
		window.showInformationMessage('Reach run successful!');
	});
	context.subscriptions.push(disposable2);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
