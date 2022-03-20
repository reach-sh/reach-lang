/* --------------------------------------------------------------------------------------------
 * Copyright for portions from https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample
 * are held by (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 *
 * Copyright (c) 2020 Eric Lau. All rights reserved.
 * Licensed under the Eclipse Public License v2.0
 *
 * Copyright (c) 2021 Reach Platform, Inc. All rights reserved.
 * Licensed under the Eclipse Public License v2.0
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import {
	commands,
	env,
	ExtensionContext,
	Terminal,
	Uri,
	window,
	workspace,
} from 'vscode';
import { exec } from 'child_process';
import { initButtons } from './buttons';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
} from 'vscode-languageclient/node';
import CHECK_FOR_UPDATES_USING from './util/checkForUpdates';
import { CommandsTreeDataProvider, DocumentationTreeDataProvider, HelpTreeDataProvider } from './CommandsTreeDataProvider';
import { terminalOptions } from "./terminalOptions";

const COMMANDS = require('../../data/commands.json');

let client: LanguageClient;

var terminal: Terminal;

const fs = require('fs');
import * as url from 'url';

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

	terminal = window.createTerminal(terminalOptions);
	const reachExecutablePath = workspace.getConfiguration().get('reachide.executableLocation') as string;
	const wf = workspace.workspaceFolders[0].uri.path || '.';
	const reachPath = path.join(wf, reachExecutablePath);
	registerCommands(context, reachPath);


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

	// Checking for updates should be
	// asynchronous and non-blocking.
	client.onReady().then(() => CHECK_FOR_UPDATES_USING(
		reachPath
	));

	// Inject association for .rsh file type
	if (workspace.workspaceFolders !== undefined) {
		rootFolder = url.fileURLToPath( workspace.workspaceFolders[0].uri.toString() );
	}
	associateRshFiles();

	window.registerTreeDataProvider('reach-commands', new CommandsTreeDataProvider());
	window.registerTreeDataProvider('reach-help', new HelpTreeDataProvider());
	window.registerTreeDataProvider('reach-docs', new DocumentationTreeDataProvider());
}

const commandHelper = (
	context: ExtensionContext,
	reachPath: string
) => (label: string) => {
	const disposable = commands.registerCommand(`reach.${label}`, () => {
		terminal.show();
		if (label === 'download Reach shell script') {
			terminal.sendText(
				'curl https://docs.reach.sh/reach -o ' +
				reachPath + ' ; chmod +x ' + reachPath
			);
		} else {
			terminal.sendText(`${reachPath} ${label}`);
		}
	});
	context.subscriptions.push(disposable);
};

const urlHelper = (
	context: ExtensionContext,
	label: string,
	url: string
) => {
	const disposable = commands.registerCommand(`reach.${label}`, () => {
		env.openExternal(Uri.parse(url));
	});
	context.subscriptions.push(disposable);
};

function registerCommands(context: ExtensionContext, reachPath: string) {
	const cmdHelper = commandHelper(context, reachPath);

	// If a command has a url, we should call
	// something like
	// urlHelper(context, 'docs', 'https://link');
	// Otherwise, call something like
	// cmdHelper('compile');
	COMMANDS.forEach((commandObject: {
		label: string; url?: string;
	}) => {
		// Extract all the properties we might need
		// from the parameter.
		const { label, url } = commandObject;
		commandObject.url ?
			urlHelper(context, label, url) :
			cmdHelper(label);
	});
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
	const settingsFile:string = `${rootFolder}${path.sep}.vscode/settings.json`;

	fs.readFile(settingsFile, function (err: any, content: string) {
		let parseJson: { [x: string]: { [x: string]: string; }; };
		try {
			parseJson = JSON.parse(content);
		} catch {
			parseJson = {};
		}
		let fileAssoc = parseJson['files.associations'];
		if (fileAssoc === undefined) {
			parseJson['files.associations'] = { '*.rsh': 'javascript' };
		} else {
			parseJson['files.associations']['*.rsh'] = 'javascript';
		}
		fs.writeFile(settingsFile, JSON.stringify(parseJson), function (err: any) {
			if (err) {
				console.error(`Could not create .vscode/settings.json: ${err}`);
				return;
			}
		});
	});
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
