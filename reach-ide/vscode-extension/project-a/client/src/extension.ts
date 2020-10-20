/* --------------------------------------------------------------------------------------------
 * Copyright for portions from https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample 
 * are held by (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * 
 * Copyright (c) 2020 Eric Lau. All rights reserved. 
 * Licensed under the Eclipse Public License v2.0
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, commands, window, env } from 'vscode';
import { exec } from 'child_process';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient';

let client: LanguageClient;

var terminal;

const fs = require('fs')

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

	const disposable3 = commands.registerCommand('reach.update', () => {
		terminal.show();
		terminal.sendText("./reach update");
	});
	context.subscriptions.push(disposable3);

	const disposable4 = commands.registerCommand('reach.get.eth.abi', async () => {
		fs.readFile(workspace.rootPath + '/build/index.main.mjs',async function(err: any, text: string){
			var pattern = /ABI: `[\s\S]*\]`/g;
			var m;
			while ((m = pattern.exec(text))) {
				m[0] = m[0].substring(6, m[0].length - 1)
				await env.clipboard.writeText(m[0]); 
				window.showInformationMessage("Copied Ethereum contract ABI to clipboard!")
			}
		})
	});
	context.subscriptions.push(disposable4);

	const disposable5 = commands.registerCommand('reach.get.eth.bytecode', () => {
		fs.readFile(workspace.rootPath + '/build/index.main.mjs',async function(err: any, text: string){
			var pattern = /Bytecode: `[\s\S]*`,/g;
			var m;
			while ((m = pattern.exec(text))) {
				m[0] = m[0].substring(11, m[0].length - 2)
				await env.clipboard.writeText(m[0]); 
				window.showInformationMessage("Copied Ethereum contract bytecode to clipboard!")
			}
		})
	});
	context.subscriptions.push(disposable5);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
