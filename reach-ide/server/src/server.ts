import { stringify } from 'querystring';
/* --------------------------------------------------------------------------------------------
 * Copyright for portions from https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample 
 * are held by (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * 
 * Copyright (c) 2020 Eric Lau. All rights reserved. 
 * Licensed under the Eclipse Public License v2.0
 * ------------------------------------------------------------------------------------------ */

import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult,
	CodeLens,
	CodeLensParams,
	CodeAction,
	CodeActionKind,
	CodeActionParams,
	CodeActionContext,
	Command,
	WorkspaceEdit,
	HoverParams,
	Hover,
	MarkedString,
	MarkupContent,
	MarkupKind, Position
} from 'vscode-languageserver';

import {
	TextDocument, Range, TextEdit
} from 'vscode-languageserver-textdocument';

// Create a connection for the server. The connection uses Node's IPC as a transport.
// Also include all preview / proposed LSP features.
let connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager. The text document manager
// supports full document sync only
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability: boolean = false;
let hasWorkspaceFolderCapability: boolean = false;
let hasDiagnosticRelatedInformationCapability: boolean = false;

const NAME: string = 'Reach IDE';

const DIAGNOSTIC_TYPE_COMPILE_ERROR: string = 'CompileError';

const {indexOfRegex, lastIndexOfRegex} = require('index-of-regex')

connection.onInitialize((params: InitializeParams) => {
	let capabilities = params.capabilities;

	// Does the client support the `workspace/configuration` request?
	// If not, we will fall back using global settings
	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	);
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that the server supports code completion
			/*completionProvider: {
				resolveProvider: true
			},
			codeLensProvider : {
				resolveProvider: true
			},
,
			hoverProvider : {
				workDoneProgress: false
			}*/
			codeActionProvider : {
				codeActionKinds : [ CodeActionKind.QuickFix ]
			}
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

interface ReachIdeSettings {
	maxNumberOfProblems: number;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ReachIdeSettings = { maxNumberOfProblems: 100 };
let globalSettings: ReachIdeSettings = defaultSettings;

// Cache the settings of all open documents
let documentSettings: Map<string, Thenable<ReachIdeSettings>> = new Map();

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear();
	} else {
		globalSettings = <ReachIdeSettings>(
			(change.settings.reachide || defaultSettings)
		);
	}

	// Revalidate all open text documents
	documents.all().forEach(validateTextDocument);
});

function getDocumentSettings(resource: string): Thenable<ReachIdeSettings> {
	if (!hasConfigurationCapability) {
		return Promise.resolve(globalSettings);
	}
	let result = documentSettings.get(resource);
	if (!result) {
		result = connection.workspace.getConfiguration({
			scopeUri: resource,
			section: 'reachide'
		});
		documentSettings.set(resource, result);
	}
	return result;
}

// Only keep settings for open documents
documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {

	// In this simple example we get the settings for every validate run.
	let settings = await getDocumentSettings(textDocument.uri);

	let diagnostics: Diagnostic[] = [];

	

	const { exec } = require("child_process");

	await exec("curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach -o reach ; chmod +x reach", (error: { message: any; }, stdout: any, stderr: any) => {
		if (error) {
			connection.console.log(`Reach download error: ${error.message}`);
			return;
		}
		if (stderr) {
			connection.console.log(`Reach download stderr: ${stderr}`);
			return;
		}
		connection.console.log(`Reach download stdout: ${stdout}`);
	});

	// TODO compile temp file instead of this current file

	connection.console.log(`BEFORE`);

	await exec("./reach compile", (error: { message: any; }, stdout: any, stderr: any) => {
		if (error) {
			connection.console.log(`FOUND Reach compile error: ${error.message}`);
			let errorLocations : ErrorLocation[] = findErrorLocations(error.message);
			let problems = 0;
			for (var i = 0; i < errorLocations.length; i++) {
				let element : ErrorLocation = errorLocations[i];
				connection.console.log(`FOR EACH LOCATION, ERROR MSG IS ` + element.errorMessage);
				if (problems < settings.maxNumberOfProblems) {
					problems++;
					
					addDiagnostic(element, `${element.errorMessage}`, 'Reach compilation encountered an error.', DiagnosticSeverity.Error, DIAGNOSTIC_TYPE_COMPILE_ERROR + element.suggestions);
					
				}
			}

			return;
		}
		if (stderr) {
			connection.console.log(`Reach compile stderr: ${stderr}`);
			return;
		}
		connection.console.log(`Reach compile stdout: ${stdout}`);
	});

	connection.console.log(`AFTER`);

		// Send the computed diagnostics to VSCode (before the above promise finishes, just to clear stuff).
		connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });

	function addDiagnostic(element: ErrorLocation, message: string, details: string, severity: DiagnosticSeverity, code: string | undefined) {
		let diagnostic: Diagnostic = {
			severity: severity,
			range: element.range,
			message: message,
			source: NAME,
			code: code
		};
		connection.console.log(`ADDING DIAGNOSTIC ` + diagnostic.range.start.line + " " + diagnostic.range.start.character + " " + diagnostic.range.end.line + " " + diagnostic.range.end.character + ", " + diagnostic.message);
		if (hasDiagnosticRelatedInformationCapability) {
			diagnostic.relatedInformation = [
				{
					location: {
						uri: textDocument.uri,
						range: Object.assign({}, diagnostic.range)
					},
					message: details
				}
			];
		}
		diagnostics.push(diagnostic);

		// Send the computed diagnostics to VSCode.
		connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
	}
}

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received an file change event');
});

export interface ErrorLocation {
    range: Range;
	errorMessage: string; // e.g. id ref: Invalid unbound identifier: declassiafy. Did you mean: ["declassify","array","assert","assume","closeTo"]
	suggestions: string; // e.g. literally this whole thing: "declassify","array","assert","assume","closeTo"
}

function findErrorLocations(compileErrors: string) : ErrorLocation[] {
	connection.console.log(`FIND ERROR LOCATIONS`);
	// GET ERROR MESSAGES BY RUNNING COMPILE


// change the below to run reach compile and get the output e.g. parse from
	/*

	WARNING: Found orphan containers (tut_ethereum-devnet_1) for this project. If you removed or renamed this service in your compose file, you can run this command with the --remove-orphans flag to clean it up.
Creating tut_reach_run ... done
reachc: error: ./index.rsh:13:23:id ref: Invalid unbound identifier: declassiafy. Did you mean: ["declassify","array","assert","assume","closeTo"]
CallStack (from HasCallStack):
  error, called at src/Reach/AST.hs:58:3 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.AST
  expect_throw, called at src/Reach/Eval.hs:441:7 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.Eval
  env_lookup, called at src/Reach/Eval.hs:1638:41 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.Eval

	*/


	//let text = textDocument.getText();
	let pattern = /error: .\/index.rsh.*/g;  // 0x then 40 hex chars then non hex char
	let m: RegExpExecArray | null;

	let problems = 0;
	let locations: ErrorLocation[] = [];
	while ((m = pattern.exec(compileErrors)) && problems < 100 /*settings.maxNumberOfProblems*/) {
		connection.console.log(`FOUND PATTERN: ${m}`);
	// ERROR MESSAGE m:
    //error: ./index.rsh:13:23:id ref: Invalid unbound identifier: declassiafy. Did you mean: ["declassify","array","assert","assume","closeTo"]

		// Get actual message portion after the line and position numbers
		var tokens = m[0].split(':');
		connection.console.log(`TOKENS: `+tokens);
		var linePos = parseInt(tokens[2]);
		var charPos = parseInt(tokens[3]);
		var actualMessage = "";
		for (var i=4; i<tokens.length; i++) {
			actualMessage += tokens[i];
			if (i < (tokens.length - 1)) {
				actualMessage += ":"; // add back the colons in between
			}
		}

		// Get list of suggestions from compiler
		const SUGGESTIONS_PREFIX = "Did you mean: [";
		const SUGGESTIONS_SUFFIX = "]";
		var indexOfSuggestions = actualMessage.indexOf(SUGGESTIONS_PREFIX);
		var suggestions = actualMessage.substring(indexOfSuggestions + SUGGESTIONS_PREFIX.length, actualMessage.lastIndexOf(SUGGESTIONS_SUFFIX));
		connection.console.log(`SUGGESTIONS: ${suggestions}`);

		// Get the problematic string (before the list of suggestions)
		var messageWithoutSuggestions = actualMessage.substring(0, indexOfSuggestions);
		let messageWithoutSuggestionsTokens : string[] = messageWithoutSuggestions.split(" ");
		var problematicString = messageWithoutSuggestionsTokens[messageWithoutSuggestionsTokens.length - 1];
		problematicString = problematicString.substring(0, problematicString.length - 1); // remove trailing period at end of sentence 
		connection.console.log(`PROBLEMATIC STRING: ${problematicString}`);

		let location: ErrorLocation = {
			range: {
				start: { line: linePos - 1, character: charPos - 1 }, // Reach compiler numbers starts at 1
				end: { line: linePos - 1, character: charPos - 1 + problematicString.length}
			},
			errorMessage: actualMessage,
			suggestions: suggestions
		};
		locations.push(location);
	}
	return locations;
}

connection.onCodeAction(
	async (_params: CodeActionParams): Promise<CodeAction[]> => {
		let codeActions : CodeAction[] = [];

		let textDocument = documents.get(_params.textDocument.uri)
		if (textDocument === undefined) {
			return codeActions;
		}
		let context : CodeActionContext = _params.context;
		let diagnostics : Diagnostic[] = context.diagnostics;

		codeActions = await getCodeActions(diagnostics, textDocument, _params);

		return codeActions;
	}
)

async function getCodeActions(diagnostics: Diagnostic[], textDocument: TextDocument, params: CodeActionParams) : Promise<CodeAction[]> {
	let codeActions : CodeAction[] = [];

	// Get quick fixes for each diagnostic
	for (let i = 0; i < diagnostics.length; i++) {

		let diagnostic = diagnostics[i];
		if (String(diagnostic.code).startsWith(DIAGNOSTIC_TYPE_COMPILE_ERROR)) {
			let title : string = "Suggested fix";
			let range : Range = diagnostic.range;
			let possibleReplacements : string = String(diagnostic.code).substring(DIAGNOSTIC_TYPE_COMPILE_ERROR.length);

			// TODO add a quickfix for each possible replacement

			// Convert list of suggestions to an array
			// Example input: "declassify","array","assert","assume","closeTo"
			var suggestionsArray = possibleReplacements.split(","); // split by commas
			for (var j=0; j<suggestionsArray.length; j++) {
				suggestionsArray[j] = suggestionsArray[j].substring(1, suggestionsArray[j].length - 1); // remove surrounding quotes

				codeActions.push(getQuickFix(diagnostic, title + ": " + suggestionsArray[j], range, suggestionsArray[j], textDocument));
			}
		}
	}

	return codeActions;
}

function getQuickFix(diagnostic:Diagnostic, title:string, range:Range, replacement:string, textDocument:TextDocument) : CodeAction {
	let textEdit : TextEdit = { 
		range: range,
		newText: replacement
	};
	let workspaceEdit : WorkspaceEdit = {
		changes: { [textDocument.uri]:[textEdit] }
	}
	let codeAction : CodeAction = { 
		title: title, 
		kind: CodeActionKind.QuickFix,
		edit: workspaceEdit,
		diagnostics: [diagnostic]
	}
	return codeAction;
}


// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
