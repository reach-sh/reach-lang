import { stringify } from 'querystring';
import { Url } from 'url';
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

import {
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
	CodeAction,
	CodeActionKind,
	CodeActionParams,
	CodeActionContext,
	WorkspaceEdit,
	HoverParams,
	Hover,
} from 'vscode-languageserver';

// edit env variable to track extension usage; see
// https://nodejs.org/api/process.html#processenv
import { env } from "process";
env.REACH_IDE = "1";

// Do this import from vscode-languageserver/node instead of
// vscode-languageserver to avoid
// "Expected 2-3 arguments, but got 1.ts(2554)
// server.d.ts(866, 202):
// An argument for 'watchDog' was not provided."
// error from TypeScript later
import { createConnection } from 'vscode-languageserver/node';

import {
	TextDocument, Range, TextEdit
} from 'vscode-languageserver-textdocument';
import { KEYWORD_TO_COMPLETION_ITEM_KIND, REACH_KEYWORDS } from './keywordCompletion';

// Do this import differently so we can add types, to avoid a
// "No index signature with a parameter of type 'string' was found... ts(7053)"
// error later. See
// https://stackoverflow.com/questions/42986950/how-to-define-import-variable-type
// Also, if we do this import this way, we don't have to add
// "resolveJsonModule": true,
// to tsconfig.json.
const KEYWORD_TO_DOCUMENTATION: { [ keyword: string ] : string } = require(
	'../../data/keywordToDocumentation.json'
);

// for "smart auto-complete" for things like
// Reach.App, Participant.Set, Array.zip, etc.
import {
	KEYWORD_WITH_PERIOD_TO_KEYWORDS_LIST,
	KEYWORD_TO_ITEM_KIND_IMPORT
} from "./mapKeywordsWithAPeriodToAKeywordList";

// Create a connection for the server. The connection uses Node's IPC as a transport.
// Also include all preview / proposed LSP features.
let connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager. The text document manager
// supports full document sync only
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability: boolean = false;
let hasWorkspaceFolderCapability: boolean = false;
let hasDiagnosticRelatedInformationCapability: boolean = false;

const DIAGNOSTIC_SOURCE: string = 'Reach';

const DID_YOU_MEAN_PREFIX = 'Did you mean: ';

let reachTempIndexFile: string;
const REACH_TEMP_FILE_NAME = "index.rsh";

const { exec } = require("child_process");

const { indexOfRegex, lastIndexOfRegex } = require('index-of-regex')

const fs = require('fs')
const os = require('os')
const path = require('path')

let tempFolder: string;

let workspaceFolder: string;

connection.onInitialize((params: InitializeParams) => {

	fs.mkdtemp(path.join(os.tmpdir(), 'reach-ide-'), (err: string, folder: string) => {
		if (err) {
			connection.console.error(err);
			throw err;
		}
		// Use a format like 'reach-ide-SUFFIX/reach-ide'
		tempFolder = path.join(folder, 'reach-ide');
		fs.mkdir(tempFolder, (err2:string) => {
			if (err2) {
				connection.console.error(err2);
				throw err2;
			}
			reachTempIndexFile = path.join(tempFolder, REACH_TEMP_FILE_NAME);
			connection.console.log("Temp folder: " + tempFolder);
		});
	});

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
			textDocumentSync: TextDocumentSyncKind.Full,
			// Tell the client that the server supports code completion
			completionProvider: {
				resolveProvider: true,
				triggerCharacters: ["."]
			},
			/*			codeLensProvider : {
							resolveProvider: true
						},
			,*/
			hoverProvider: {
				workDoneProgress: false
			},
			codeActionProvider: {
				codeActionKinds: [CodeActionKind.QuickFix]
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

const DEFAULT_MAX_PROBLEMS = 100;

interface ReachIdeSettings {
	maxNumberOfProblems: number;
	executableLocation: string;
}

let defaultSettings: ReachIdeSettings;
let globalSettings: ReachIdeSettings;

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

documents.onDidOpen((event) => {
	connection.console.log(`Document opened: ${event.document.uri}`);
	let doc = event.document.uri;
	let folderUrl : URL = new URL(doc.substring(0, doc.lastIndexOf('/')));
	workspaceFolder = folderUrl.pathname;
	connection.console.log(`Workspace folder: ${workspaceFolder}`);

	// The global settings, used when the `workspace/configuration` request is not supported by the client.
	// Please note that this is not the case when using this server with the client provided in this example
	// but could happen with other clients.
	defaultSettings = { maxNumberOfProblems: DEFAULT_MAX_PROBLEMS, executableLocation: path.join(workspaceFolder, "reach") };

	globalSettings = defaultSettings;
});

// Only keep settings for open documents
documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	validateTextDocument(change.document);
});

let	theCompilerIsCompiling 	= false,
	weNeedToCompileAgain 	= false;

async function validateTextDocument(textDocument: TextDocument): Promise<void> {

	let textDocumentFromURI = documents.get(textDocument.uri)
	let textDocumentContents = textDocumentFromURI?.getText()

	// In this simple example we get the settings for every validate run.
	let settings = await getDocumentSettings(textDocument.uri);

	let diagnostics: Diagnostic[] = [];

	// Download the Reach shell script if it does not exist
	try {
		if (fs.existsSync('./reach')) {
			connection.console.log("Reach shell script exists");
		} else {
			connection.console.log("Reach shell script not found, downloading now...");
			await exec("curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach -o reach ; chmod +x reach", (error: { message: any; }, stdout: any, stderr: any) => {
				if (error) {
					connection.console.error(`Reach download error: ${error.message}`);
					return;
				}
				if (stderr) {
					connection.console.error(`Reach download stderr: ${stderr}`);
					return;
				}
				connection.console.log(`Reach download stdout: ${stdout}`);
			});
		}
	} catch (err) {
		connection.console.log("Failed to check if Reach shell scripts exists, downloading anyways...");
		await exec("curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach -o reach ; chmod +x reach", (error: { message: any; }, stdout: any, stderr: any) => {
			if (error) {
				connection.console.error(`Reach download error: ${error.message}`);
				return;
			}
			if (stderr) {
				connection.console.error(`Reach download stderr: ${stderr}`);
				return;
			}
			connection.console.log(`Reach download stdout: ${stdout}`);
		});
	}

	// Compile temp file instead of this current file

	fs.writeFile(reachTempIndexFile, textDocumentContents, function (err: any) {
		if (err) {
			connection.console.error(`Failed to write temp source file: ${err}`);
			return;
		}
		connection.console.log(`Temp source file ${reachTempIndexFile} saved!`);
	});

	const exeLoc = settings?.executableLocation?.trim() || '';
	const reachPath = (exeLoc == '' || exeLoc == './reach')
		? path.join(workspaceFolder, "reach")
		: exeLoc;

	if (theCompilerIsCompiling) {
		weNeedToCompileAgain = true;
		console.debug(
			"Compilation already in process; will recompile",
			new Date().toLocaleTimeString()
		);
		return;
	}

	theCompilerIsCompiling = true;

	console.debug(
		"Starting compilation at",
		new Date().toLocaleTimeString()
	);
	await exec("cd " + tempFolder + " && " + reachPath + " compile " + REACH_TEMP_FILE_NAME + " --error-format-json --stop-after-eval", (error: { message: any; }, stdout: any, stderr: any) => {
		// This callback function should execute exactly
		// when this compilation command has finished.
		// "child_process.exec(): spawns a shell...
		// passing the stdout and stderr to a callback
		// function when complete". See
		// https://nodejs.org/api/child_process.html#child-process
		console.debug(
			"Compilation should now have finished.",
			new Date().toLocaleTimeString()
		);
		theCompilerIsCompiling = false;
		if (weNeedToCompileAgain) {
			weNeedToCompileAgain = false;
			validateTextDocument(textDocument);
			return;
		}

		if (error) {
			connection.console.log(`Found compile error: ${error.message}`);
			const errorLocations: ErrorLocation[] = findErrorLocations(error.message);
			let problems = 0;
			errorLocations.forEach(err => {
				connection.console.log(`Displaying error message: ` + err.errorMessage);
				let maxProblems = settings?.maxNumberOfProblems || DEFAULT_MAX_PROBLEMS;
				if (problems < maxProblems) {
					problems++;
					addDiagnostic(
						err,
						`${err.errorMessage}`,
						'Reach compilation encountered an error.',
						DiagnosticSeverity.Error,
						err.code,
						err.suggestions,
						DIAGNOSTIC_SOURCE
					);
				}
			});

			return;
		}
		/** This doesn't seem to show anything useful at the moment
		if (stderr) {
			connection.console.error(`Reach compiler: ${stderr}`);
			return;
		}
		*/
		connection.console.log(`Reach compiler output: ${stdout}`);
	});

	// Send the computed diagnostics to VSCode (before the above promise finishes, just to clear stuff).
	connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });

	function addDiagnostic(element: ErrorLocation, message: string, details: string, severity: DiagnosticSeverity, code: string | undefined, suggestions: string[], source: string) {
		const href = `https://docs.reach.sh/${code}.html`;
		let diagnostic: Diagnostic = {
			severity: severity,
			range: element.range,
			message: message,
			source,
			code: code,
			codeDescription: {
				href
			}
		};
		if (hasDiagnosticRelatedInformationCapability) {
			diagnostic.relatedInformation = [
				{
					location: {
						uri: textDocument.uri,
						range: Object.assign({}, diagnostic.range)
					},
					message: suggestions.length ? DID_YOU_MEAN_PREFIX + suggestions : details
				},
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
	code: string;
	range: Range;
	errorMessage: string; // e.g. id ref: Invalid unbound identifier: declassify.
	suggestions: string[]; // e.g. literally this whole thing: "declassify","array","assert","assume","closeTo"
}

// Based on
// https://github.com/reach-sh/reach-lang/blob/master/hs/src/Reach/AST/Base.hs#L84
type ReachCompilerErrorJSON = {
	ce_position: number[];
	ce_suggestions: string[];
	ce_errorMessage: string;
	ce_offendingToken: string | null;
	ce_errorCode: string;
}

function findErrorLocations(compileErrors: string): ErrorLocation[] {

	// Example output to parse
	/*
	WARNING: Found orphan containers (tut_ethereum-devnet_1) for this project. If you removed or renamed this service in your compose file, you can run this command with the --remove-orphans flag to clean it up.
Creating tut_reach_run ... done
reachc: error: ./.index.rsh:18:23:id ref: Invalid unbound identifier: declaaaaaaaaassify. Did you mean: ["declassify","addressEq","array","assert","assume"]
CallStack (from HasCallStack):
	error, called at src/Reach/AST.hs:58:3 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.AST
	expect_throw, called at src/Reach/Eval.hs:441:7 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.Eval
	env_lookup, called at src/Reach/Eval.hs:1638:41 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.Eval
	*/

	let pattern = /error: .*/g  // look for error string
	let m: RegExpExecArray | null;

	let problems = 0;
	let locations: ErrorLocation[] = [];
	while ((m = pattern.exec(compileErrors)) && problems < 100 /*settings.maxNumberOfProblems*/) {
		connection.console.log(`Found pattern: ${m}`);

		// ERROR MESSAGE m: error: <json>
		const errorJson: ReachCompilerErrorJSON = JSON.parse(m[0].substring(7));

		//connection.console.log(`Tokens: ` + tokens);
		const linePos = errorJson.ce_position[0] - 1;
		const charPos = errorJson.ce_position[1] - 1;
		const suggestions = errorJson.ce_suggestions;
		const actualMessage = errorJson.ce_errorMessage;
		const offendingToken = errorJson.ce_offendingToken;
		const reachCompilerErrorCode = errorJson.ce_errorCode;

		const start ={ line: linePos, character: charPos };
		const end = offendingToken ?
				{ line: linePos, character: charPos + offendingToken.length }
			:	{ line: linePos + 1, character: 0 };

		// App options currently have error position at the `:` of a json field: `<k> : <v>`.
		// In other words, the highlighting for error "RE0013" is weird;
		// it highlights a colon instead of a word, so we have special
		// logic for this error code that we don't have for others.
		if (reachCompilerErrorCode === "RE0013") {
			// If we have this error code, an offendingToken will exist,
			// which is why we can assert to TypeScript that
			// offendingToken is non-null with the "!" operator.
			// https://stackoverflow.com/questions/38874928/operator-in-typescript-after-object-method
			start.character -= offendingToken!.length;
			end.character   -= offendingToken!.length;
		}

		let location: ErrorLocation = {
			code: reachCompilerErrorCode,
			range: { start: start, end: end },
			errorMessage: actualMessage,
			suggestions: suggestions
		};

		locations.push(location);
	}
	return locations;
}

connection.onCodeAction(
	async (_params: CodeActionParams): Promise<CodeAction[]> => {
		let codeActions: CodeAction[] = [];

		let textDocument = documents.get(_params.textDocument.uri)
		if (textDocument === undefined) {
			return codeActions;
		}
		let context: CodeActionContext = _params.context;
		let diagnostics: Diagnostic[] = context.diagnostics;

		codeActions = await getCodeActions(diagnostics, textDocument, _params);

		return codeActions;
	}
)

async function getCodeActions(diagnostics: Diagnostic[], textDocument: TextDocument, params: CodeActionParams): Promise<CodeAction[]> {
	let codeActions: CodeAction[] = [];
	const labelPrefix = 'Replace with ';

	// Get quick fixes for each diagnostic
	diagnostics.forEach(diagnostic => {
		const { range, relatedInformation } = diagnostic;
		const message = (relatedInformation || [])[0]?.message || "";
		// Diagnostics are either suggestions or generic 'Reach compilation error encountered' messages
		if (message.startsWith(DID_YOU_MEAN_PREFIX)) {
			const suggestions = message.substring(DID_YOU_MEAN_PREFIX.length).split(',');
			suggestions.forEach(suggestion => {
				codeActions.push(getQuickFix(diagnostic, labelPrefix + suggestion, range, suggestion, textDocument));
			});
		} else if (diagnostic.code === "RE0048") {
			const title = "Add Reach program header.";
			const newText: string = "'reach 0.1';\n\n";
			const textEdit: TextEdit = {
				newText,
				range
			};
			const changes = {
				[ textDocument.uri ]: [ textEdit ]
			};
			const edit: WorkspaceEdit = {
				changes
			};
			const codeAction: CodeAction = {
				edit,
				diagnostics: [ diagnostic ],
				kind: CodeActionKind.QuickFix,
				title
			};
			codeActions.push(codeAction);
		}
	});

	return codeActions;
}

function getQuickFix(diagnostic: Diagnostic, title: string, range: Range, replacement: string, textDocument: TextDocument): CodeAction {
	let textEdit: TextEdit = {
		range: range,
		newText: replacement
	};
	let workspaceEdit: WorkspaceEdit = {
		changes: { [textDocument.uri]: [textEdit] }
	}
	let codeAction: CodeAction = {
		title: title,
		kind: CodeActionKind.QuickFix,
		edit: workspaceEdit,
		diagnostics: [diagnostic]
	}
	return codeAction;
}

const GET_KEYWORDS_FOR = (
	currentLine: string | undefined,
	map: { [key: string]: string[]; },
	fallbackCompletionList: string[]
): string[] => {
	for (const keywordWithPeriod in map)
		if (currentLine?.endsWith(keywordWithPeriod))
			return map[keywordWithPeriod];
	
	return fallbackCompletionList;
};

// This handler provides the initial list of the
// completion items.
connection.onCompletion((
	textDocumentPosition: TextDocumentPositionParams
): CompletionItem[] => {
	const {
		textDocument,
		position
	} = textDocumentPosition;

	console.debug(position);

	const currentDocument = documents.get(
		textDocument.uri
	);
	
	// Grab the current line.
	const currentLine = currentDocument?.getText(
		{
			start: {
				line: position.line,
				character: 0
			},
			end: position
		}
	);

	console.debug(currentLine);

	// Do we have "smart auto-complete" for this
	// keyword? If so, just give the "smart"
	// suggestions. Otherwise, give the regular
	// suggestions.
	const keywords: string[] = GET_KEYWORDS_FOR(
		currentLine,
		KEYWORD_WITH_PERIOD_TO_KEYWORDS_LIST,
		REACH_KEYWORDS
	);
		
	// The passed parameter contains the position of
	// the text document in which code complete got
	// requested.
	return keywords.map(keyword => ({
		label: keyword,
		kind: KEYWORD_TO_COMPLETION_ITEM_KIND[
			keyword
		] || CompletionItemKind.Text,
		data: undefined,
		detail: `(${
			KEYWORD_TO_ITEM_KIND_IMPORT[keyword]
		})`,
		documentation: {
			kind: 'markdown',
			value: getReachKeywordMarkdown(keyword)
		},
	}));
});

function insertSnippet(_textDocumentPosition: TextDocumentPositionParams, snippetText: string, completionItems: CompletionItem[], imports: string | undefined, label: string, sortOrder: number) {
	let textEdit: TextEdit = {
		range: {
			start: _textDocumentPosition.position,
			end: _textDocumentPosition.position
		},
		newText: snippetText
	};
	let completionItem: CompletionItem = {
		label: label,
		kind: CompletionItemKind.Snippet,
		data: undefined,
		textEdit: textEdit,
		sortText: String(sortOrder)
	};
	// check if imports should be added
	let textDocument = documents.get(_textDocumentPosition.textDocument.uri)
	let textDocumentContents = textDocument?.getText()
	if (imports !== undefined && (textDocumentContents === undefined || !String(textDocumentContents).includes(imports))) {
		let additionalTextEdit = {
			range: {
				start: { line: 0, character: 0 },
				end: { line: 0, character: 0 }
			},
			newText: imports
		};
		completionItem.additionalTextEdits = [additionalTextEdit]
	}

	completionItems.push(completionItem);
}

// This handler resolves additional information for the item selected in
// the completion list.
connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		return item;
	}
);

connection.onHover(

	async (_params: HoverParams): Promise<Hover> => {
		let textDocument = documents.get(_params.textDocument.uri)
		let position = _params.position
		let hover: Hover = {
			contents: ""
		}
		if (textDocument !== undefined) {
			var start = {
				line: position.line,
				character: 0,
			};
			var end = {
				line: position.line + 1,
				character: 0,
			};
			var text = textDocument.getText({ start, end });
			var index = textDocument.offsetAt(position) - textDocument.offsetAt(start);

			var word = getWord(text, index, true);
			if (isReachKeyword(word)) {
				let buf = await getReachKeywordMarkdown(word);
				hover.contents = buf;
				return hover;
			}

			var word = getWord(text, index, false);
			if (isReachKeyword(word)) {
				let buf = await getReachKeywordMarkdown(word);
				hover.contents = buf;
				return hover;
			}
		}
		return hover;
	}

);

function isReachKeyword(word: string): boolean {
	return KEYWORD_TO_COMPLETION_ITEM_KIND[word] != undefined;
}

function getReachKeywordMarkdown(word: string): string {
	// source: https://docs.reach.sh/ref-programs-module.html#%28tech._source._file%29
	// then input to: https://euangoddard.github.io/clipboard2markdown/
	// then input to: https://www.freeformatter.com/javascript-escape.html
	// If we don't have documentation for `word`,
	// then just set `buf` to the empty string.
	let buf = KEYWORD_TO_DOCUMENTATION[word] || '';
	buf = buf.replace(/`/g, ''); // Get rid of all code formatting which messes up hyperlinks
	return buf;
}

function getWord(text: string, index: number, includeDot: boolean) {
	var beginSubstring = text.substring(0, index);

	var endSubstring = text.substring(index, text.length);
	var boundaryRegex;
	if (includeDot) {
		boundaryRegex = /[^0-9a-zA-Z.]{1}/g; // boundaries are: not alphanumeric or dot
	} else {
		boundaryRegex = /[^0-9a-zA-Z]{1}/g; // boundaries are: not alphanumeric or dot
	}
	var first = lastIndexOfRegex(beginSubstring, boundaryRegex) + 1;
	var last = index + indexOfRegex(endSubstring, boundaryRegex);

	return text.substring(first !== -1 ? first : 0, last !== -1 ? last : text.length - 1);
}

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
