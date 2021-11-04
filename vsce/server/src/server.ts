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
	CodeAction,
	CodeActionKind,
	CodeActionParams,
	CodeActionContext,
	WorkspaceEdit,
	HoverParams,
	Hover,
} from 'vscode-languageserver';

import {
	TextDocument, Range, TextEdit
} from 'vscode-languageserver-textdocument';
import { reachCompletionKind, reachKeywords } from './keywordCompletion';

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

const DIAGNOSTIC_TYPE_COMPILE_ERROR: string = 'Compile Error';

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
	await exec("cd " + tempFolder + " && " + reachPath + " compile " + REACH_TEMP_FILE_NAME + " --error-format-json", (error: { message: any; }, stdout: any, stderr: any) => {
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
						DIAGNOSTIC_TYPE_COMPILE_ERROR,
						err.suggestions
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

	function addDiagnostic(element: ErrorLocation, message: string, details: string, severity: DiagnosticSeverity, code: string | undefined, suggestions: string[]) {
		let diagnostic: Diagnostic = {
			severity: severity,
			range: element.range,
			message: message,
			source: NAME,
			code: code
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
type ReachCompilerErrorJSON {
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


// This handler provides the initial list of the completion items.
connection.onCompletion(
	(_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
		// The passed parameter contains the position of the text document in
		// which code complete got requested.
		return reachKeywords.map(kwd => ({
			label: kwd,
			kind: reachCompletionKind[kwd] || CompletionItemKind.Text,
			data: undefined,
			detail: kwd,
			documentation: {
				kind: 'markdown',
				value: getReachKeywordMarkdown(kwd)
			},
		}));
	}
);

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
	return reachCompletionKind[word] != undefined;
}

function getReachKeywordMarkdown(word: string): string {
	// source: https://docs.reach.sh/ref-programs-module.html#%28tech._source._file%29
	// then input to: https://euangoddard.github.io/clipboard2markdown/
	// then input to: https://www.freeformatter.com/javascript-escape.html
	var buf: string = "";
	if (word == 'export') {
		buf = "#### export\r\n\r\nModule-level\u00A0[identifier definitions](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._identifier._definition%29)\u00A0may be\u00A0exported by writing\u00A0`export`\u00A0in front of them. For example,\r\n\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 1;\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) [a, b, ...more] = [ 0, 1, 2, 3, 4 ];\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [function](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28function%29%29%29) add1(x) { [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) x [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 1; };\r\n\r\nare valid\u00A0[exports](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29).\r\n\r\nModule-level identifiers may also be\u00A0[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29)ed after the fact, and may be renamed during export. For example:\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) w = 2;\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) z = 0;\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) {w, z as zero};\r\n\r\nIdentifiers from other modules may be re-exported (and renamed), even if they are not imported in the current module. For example:\r\n\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) {u, x as other_x} [from](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28from%29%29%29) \'.\/other-module.rsh\';\r\n\r\nAn\u00A0[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29)ed identifier in a given\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29)\u00A0may be\u00A0[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._import%29)ed by other\u00A0[modules](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29).";
	} else if (word == 'import') {
		buf = "#### import\r\n\r\nimport \'games-of-chance.rsh\';\r\n\r\nWhen a\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29),\u00A0X, contains an\u00A0import, written\u00A0`[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28import%29%29%29) \"LIB.rsh\";`, then the path\u00A0\"LIB.rsh\"\u00A0must resolve to another Reach\u00A0[source file](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._source._file%29). The\u00A0[exports](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29)\u00A0from the\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29)\u00A0defined by\u00A0\"LIB.rsh\"\u00A0are included in the set of\u00A0[bound identifier](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._bound._identifier%29)s in\u00A0X.\r\n\r\n[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28import%29%29%29) {flipCoin, rollDice as d6} from \'games-of-chance.rsh\';\r\n\r\nImport statements may limit or rename the imported\u00A0[identifiers](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._identifier%29).\r\n\r\n[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28import%29%29%29) [*](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2A%29%29%29) as gamesOfChance [from](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28from%29%29%29) \'games-of-chance.rsh\';\r\n\r\nImports may instead bind the entire\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29)\u00A0to a single\u00A0[identifier](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._identifier%29), which is an\u00A0[object](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._object%29)\u00A0with\u00A0[fields](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._field%29)\u00A0corresponding to that\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29)\'s\u00A0[exports](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29).\r\n\r\n[Import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._import%29)\u00A0cycles are\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29).\r\n\r\nThe path given to an\u00A0[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._import%29)\u00A0may\u00A0not\u00A0include\u00A0..\u00A0to specify files outside the current directory\u00A0nor\u00A0may it be an absolute path.\r\n\r\nIt\u00A0must\u00A0be a relative path, which is resolved relative to the parent directory of the\u00A0[source file](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._source._file%29)\u00A0in which they appear.";
	} else if (word == 'Reach.App') {
		buf = "### Reach.App\r\n\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) main =\r\n  Reach.App({}, [[\"A\", {displayResult: [Fun](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Fun%29%29%29)(Int, [Null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Null%29%29%29))}]], (A) [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n    [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) result = 0;\r\n    A.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) { [interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).displayResult(result); })\r\n    [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) result;\r\n  });\r\n\r\nReach.App\u00A0is a function which accepts three arguments:\u00A0`options`,\u00A0`participantDefinitions`, and\u00A0`program`.\r\n\r\nThe\u00A0`options`\u00A0must be an object. It supports the following options:\r\n\r\n|\r\n\r\n`deployMode`\r\n\r\n |  |\r\n\r\n`\'constructor\'`\u00A0(default) or\u00A0`\'firstMsg\'`\r\n\r\n |  |\r\n\r\nDetermines whether\u00A0[contract](https:\/\/docs.reach.sh\/ref-model.html#%28tech._contract%29)\u00A0should be\u00A0[deploy](https:\/\/docs.reach.sh\/ref-model.html#%28tech._deploy%29)ed independently (`\'constructor\'`) or as part of the first\u00A0[publication](https:\/\/docs.reach.sh\/ref-model.html#%28tech._publication%29)\u00A0(`\'firstMsg\'`). If deployed as part of the first publication, then the first publication must precede all uses of\u00A0`[wait](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28wait%29%29%29)`\u00A0and\u00A0`.[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)`. See\u00A0[the guide on deployment modes](https:\/\/docs.reach.sh\/guide-deploymode.html)\u00A0for a discussion of why to choose a particular mode.\r\n\r\n |\r\n|  |  |  |  |  |\r\n|\r\n\r\n`verifyOverflow`\r\n\r\n |  |\r\n\r\n`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0or\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`\u00A0(default)\r\n\r\n |  |\r\n\r\nDetermines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29).max`. This defaults to\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`, because it is onerous to verify. We recommend turning it on before final deployment, but leaving it off during development. When it is\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`,\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0will ensure that overflows do not actually occur on the network.\r\n\r\n |\r\n|  |  |  |  |  |\r\n|\r\n\r\n`verifyPerConnector`\r\n\r\n |  |\r\n\r\n`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0or\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`\u00A0(default)\r\n\r\n |  |\r\n\r\nDetermines whether verification is done per connector, or once for a generic connector. When this is\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`, then connector-specific constants, like\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29).max`, will be instatiated to literal numbers. This concretization of these constants can induce performance degradation in the verifier.\r\n\r\n |\r\n|  |  |  |  |  |\r\n|\r\n\r\n`connectors`\r\n\r\n |  |\r\n\r\n`[ETH, ALGO]`\u00A0(default)\r\n\r\n |  |\r\n\r\nA tuple of the\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0that the application should be compiled for. By default, all available\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0are chosen.\r\n\r\n |\r\n\r\nThe\u00A0`participantDefinitions`\u00A0argument is an tuple of tuples. Each tuple is a pair of\u00A0`participantName`\u00A0and\u00A0`participantInteractInterface`.\u00A0`participantName`\u00A0is a string which indicates the name of the participant function in the generated\u00A0[backend](https:\/\/docs.reach.sh\/ref-model.html#%28tech._backend%29)\u00A0code. Each\u00A0`participantName`\u00A0must be unique.\u00A0`participantInteractInterface`\u00A0is a\u00A0participant interact interface, an object where each field indicates the type of a function or value which must be provided to the\u00A0[backend](https:\/\/docs.reach.sh\/ref-model.html#%28tech._backend%29)\u00A0by the\u00A0[frontend](https:\/\/docs.reach.sh\/ref-model.html#%28tech._frontend%29)\u00A0for\u00A0[interact](https:\/\/docs.reach.sh\/ref-model.html#%28tech._interact%29)ing with the participant.\r\n\r\nThe\u00A0`program`\u00A0argument must be a syntactic\u00A0[arrow expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._arrow._expression%29). The arguments to this arrow must match the number and order of\u00A0`participantDefinitions`. The function body is the program to be\u00A0[compile](https:\/\/docs.reach.sh\/ref-model.html#%28tech._compile%29)d. It specifies a\u00A0[step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._step%29), which means its content is specified by\u00A0[Steps](https:\/\/docs.reach.sh\/ref-programs-step.html).\r\n\r\nIf the result of\u00A0`[Reach](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28.Reach%29%29%29).[App](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28.App%29%29%29)`\u00A0is eventually bound to an identifier that is\u00A0[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29)ed, then it may be a target given to the compiler, as discussed in\u00A0[the section on usage](https:\/\/docs.reach.sh\/ref-usage.html#%28part._ref-usage-compile%29).";
	} else if (word == 'only' || word == 'each') {
		buf = "### only\u00A0and\u00A0each\r\n\r\nAlice.only(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) pretzel = [interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).random(); });\r\n\r\nA\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29)\u00A0statement is written\u00A0`PART.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) BLOCK)`, where\u00A0`PART`\u00A0is a\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0identifier and\u00A0`BLOCK`\u00A0is a\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29). Any bindings defined within the\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29)\u00A0of a\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29)\u00A0are available in the\u00A0[statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._statement%29)\'s\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29)\u00A0as new\u00A0[local state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._state%29). For example,\r\n\r\nAlice.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 3; });\r\nAlice.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) y = x [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 1; });\r\n\r\nis a\u00A0[valid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._valid%29)\u00A0program where\u00A0`Alice`\'s\u00A0[local state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._state%29)\u00A0includes the\u00A0[private](https:\/\/docs.reach.sh\/ref-model.html#%28tech._private%29)\u00A0values\u00A0`x`\u00A0(bound to\u00A0`3`) and\u00A0`y`\u00A0(bound to\u00A0`4`). However, such bindings are\u00A0not\u00A0[consensus state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._state%29), so they are purely\u00A0[local state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._state%29). For example,\r\n\r\nAlice.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 3; });\r\nBob.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) y = x [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 1; });\r\n\r\nis an\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29)\u00A0program, because\u00A0`Bob`\u00A0does not know\u00A0`x`.\r\n\r\n---\r\n\r\neach([Alice, Bob], () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) pretzel = [interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).random(); });\r\n\r\nAn\u00A0each\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29)\u00A0statement can be written as\u00A0`[each](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28each%29%29%29)(PART_TUPLE () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) BLOCK)`, where\u00A0`PART_TUPLE`\u00A0is a tuple of\u00A0[participants](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0and\u00A0`BLOCK`\u00A0is a\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29). It is an abbreviation of many\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29)\u00A0statements that could have been written with\u00A0`[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)`."
	} else if (word == 'publish' || word == 'pay' || word == 'timeout') {
		buf = "### publish,\u00A0pay, and\u00A0timeout\r\n\r\nAlice.publish(wagerAmount)\r\n     .pay(wagerAmount)\r\n     .timeout(DELAY, () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n       Bob.[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)();\r\n       [commit](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28commit%29%29%29)();\r\n       [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) [false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29); });\r\n\r\nAlice.[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)(wagerAmount)\r\n     .[pay](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28pay%29%29%29)(wagerAmount)\r\n     .[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)(DELAY, [closeTo](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28close.To%29%29%29)(Bob, [false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)));\r\n\r\nA\u00A0[consensus transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._transfer%29)\u00A0is written\u00A0`PART.[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)(ID_0, ..., ID_n).[pay](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28pay%29%29%29)(PAY_EXPR).[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)(DELAY_EXPR, () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) TIMEOUT_BLOCK)`, where\u00A0`PART`\u00A0is a\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0identifier,\u00A0`ID_0`\u00A0through\u00A0`ID_n`\u00A0are identifiers for\u00A0`PART`\'s\u00A0[public](https:\/\/docs.reach.sh\/ref-model.html#%28tech._public%29)\u00A0[local state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._state%29),\u00A0`PAY_EXPR`\u00A0is a\u00A0[public](https:\/\/docs.reach.sh\/ref-model.html#%28tech._public%29)\u00A0[expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0evaluating to an amount of\u00A0[network tokens](https:\/\/docs.reach.sh\/ref-model.html#%28tech._network._token%29),\u00A0`DELAY_EXPR`\u00A0is a\u00A0[public](https:\/\/docs.reach.sh\/ref-model.html#%28tech._public%29)\u00A0[expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0that depends on only\u00A0[consensus state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._state%29)\u00A0and evaluates to a\u00A0[time delta](https:\/\/docs.reach.sh\/ref-model.html#%28tech._time._delta%29)\u00A0represented by a natural number,\u00A0`TIMEOUT_BLOCK`\u00A0is a\u00A0[timeout](https:\/\/docs.reach.sh\/ref-model.html#%28tech._timeout%29)\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29), which will be executed after\u00A0`DELAY_EXPR`\u00A0units of\u00A0[time](https:\/\/docs.reach.sh\/ref-model.html#%28tech._time%29)\u00A0have passed from the end of the last\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29)\u00A0without\u00A0`PART`\u00A0executing this\u00A0[consensus transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._transfer%29). The\u00A0[continuation](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._continuation%29)\u00A0of a\u00A0[consensus transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._transfer%29)\u00A0[statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._statement%29)\u00A0is a\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29), which is finalized with a\u00A0[commit statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._commit._statement%29). The\u00A0[continuation](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._continuation%29)\u00A0of a timeout block is the same as the continuation as the continuation as the function the timeout occurs within.\r\n\r\n> > > See\u00A0[the guide section on non-participation](https:\/\/docs.reach.sh\/guide-timeout.html)\u00A0to undertand when to use timeouts and how to use them most effectively.\r\n\r\nThe\u00A0`[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)`\u00A0component exclusive-or the\u00A0`[pay](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28pay%29%29%29)`\u00A0component may be omitted, if either there is no\u00A0[publication](https:\/\/docs.reach.sh\/ref-model.html#%28tech._publication%29)\u00A0or no\u00A0[transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._transfer%29)\u00A0of\u00A0[network tokens](https:\/\/docs.reach.sh\/ref-model.html#%28tech._network._token%29)\u00A0to accompany this\u00A0[consensus transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._transfer%29). The\u00A0`[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)`\u00A0component may always be omitted. Each component may occur in any order. For example, the following are all\u00A0[valid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._valid%29):\r\n\r\nAlice.[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)(coinFlip);\r\n\r\nAlice.[pay](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28pay%29%29%29)(penaltyAmount);\r\n\r\nAlice.[pay](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28pay%29%29%29)(penaltyAmount).[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)(coinFlip);\r\n\r\nAlice.[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)(coinFlip)\r\n     .[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)(DELAY, () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) [closeTo](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28close.To%29%29%29)(Bob, () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) [exit](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28exit%29%29%29)()));\r\n\r\nAlice.[pay](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28pay%29%29%29)(penaltyAmount)\r\n     .[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)(DELAY, () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n       Bob.[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)();\r\n       [commit](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28commit%29%29%29)();\r\n       [exit](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28exit%29%29%29)(); });\r\n\r\nIf the named participant has not yet\u00A0[join](https:\/\/docs.reach.sh\/ref-model.html#%28tech._join%29)ed the application, then this statement has the effect of them\u00A0[join](https:\/\/docs.reach.sh\/ref-model.html#%28tech._join%29)ing, after which\u00A0`PART`\u00A0may be used as a\u00A0[address](https:\/\/docs.reach.sh\/ref-model.html#%28tech._addres%29).";
	} else if (word == 'wait') {
		buf = "### wait\r\n\r\nwait(AMT);\r\n\r\nA\u00A0wait statement, written\u00A0`[wait](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28wait%29%29%29)(AMOUNT);`, delays the computation until\u00A0`AMOUNT`\u00A0[time delta](https:\/\/docs.reach.sh\/ref-model.html#%28tech._time._delta%29)\u00A0units have passed. It may only occur in a\u00A0[step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._step%29).";
	} else if (word == 'exit') {
		buf = "### exit\r\n\r\nexit();\r\n\r\nAn\u00A0exit statement, written\u00A0`[exit](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28exit%29%29%29)();`, halts the computation. It is a\u00A0[terminator statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._terminator._statement%29), so it must have an empty\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29). It may only occur in a\u00A0[step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._step%29).";
	} else if (word == 'unknowable') {
		buf = "### unknowable\r\n\r\nunknowable( Notter, Knower(expr_0, ..., expr_N), [msg] )\r\n\r\nA\u00A0[knowledge assertion](https:\/\/docs.reach.sh\/ref-model.html#%28tech._knowledge._assertion%29)\u00A0that the\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0`Notter`\u00A0does not\u00A0know the results of the evaluations of expressions\u00A0`expr_0`\u00A0through\u00A0`expr_N`, but that the\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0`Knower`\u00A0does\u00A0know those values. It accepts an optional bytes argument, which is included in any reported violation.";
	} else if (word == 'closeTo') {
		buf = "### closeTo\r\n\r\ncloseTo( Who, after )\r\n\r\nReturns has\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0`Who`\u00A0make a\u00A0[publication](https:\/\/docs.reach.sh\/ref-model.html#%28tech._publication%29), then\u00A0[transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._transfer%29)\u00A0the\u00A0`[balance](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28balance%29%29%29)()`\u00A0to\u00A0`Who`\u00A0and end the\u00A0[DApp](https:\/\/docs.reach.sh\/ref-model.html#%28tech._dapp%29)\u00A0after executing the function\u00A0`after`\u00A0in a\u00A0[step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._step%29).";
	} else if (word == 'interact') {
		buf = "### interact\r\n\r\ninteract.amount\r\n[interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).notify(handA, handB)\r\n[interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).chooseAmount(heap1, heap2)\r\n\r\nAn\u00A0interaction expression, written\u00A0`[interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).METHOD(EXPR_0, ..., EXPR_n)`, where\u00A0`METHOD`\u00A0is an identifier bound in the\u00A0[participant interact interface](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._participant._interact._interface%29)\u00A0to a function type, and\u00A0`EXPR_0`\u00A0through\u00A0`EXPR_n`\u00A0are\u00A0[expressions](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0that evaluates to the result of an\u00A0[interact](https:\/\/docs.reach.sh\/ref-model.html#%28tech._interact%29)ion with a\u00A0[frontend](https:\/\/docs.reach.sh\/ref-model.html#%28tech._frontend%29)\u00A0that receives the evaluation of the\u00A0`n`\u00A0[expressions](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0and sends a\u00A0[value](https:\/\/docs.reach.sh\/ref-model.html#%28tech._value%29).\r\n\r\nAn\u00A0[interaction expression](https:\/\/docs.reach.sh\/ref-programs-local.html#%28tech._interaction._expression%29)\u00A0may also be written\u00A0`[interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).KEY`, where\u00A0`KEY`\u00A0is bound in the\u00A0[participant interact interface](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._participant._interact._interface%29)\u00A0to a non-function type.\r\n\r\nAn\u00A0[interaction expression](https:\/\/docs.reach.sh\/ref-programs-local.html#%28tech._interaction._expression%29)\u00A0may only occur in a\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29).";
	} else if (word == 'assume') {
		buf = "### assume\r\n\r\nassume( claim, [msg] )\r\n\r\nAn\u00A0[assumption](https:\/\/docs.reach.sh\/ref-model.html#%28tech._assumption%29)\u00A0where\u00A0`claim`\u00A0evaluates to\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0with\u00A0[honest](https:\/\/docs.reach.sh\/ref-model.html#%28tech._honest%29)\u00A0[frontends](https:\/\/docs.reach.sh\/ref-model.html#%28tech._frontend%29). This may only appear in a\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29). It accepts an optional bytes argument, which is included in any reported violation.";
	} else if (word == 'declassify') {
		buf = "### declassify\r\n\r\ndeclassify( arg )\r\n\r\nThe\u00A0declassify\u00A0primitive performs a\u00A0[declassification](https:\/\/docs.reach.sh\/ref-model.html#%28tech._declassification%29)\u00A0of the given argument.";
	} else if (word == 'makeCommitment') {
		buf = "### makeCommitment\r\n\r\nmakeCommitment( [interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29), x )\r\n\r\nReturns two values,\u00A0`[ commitment, salt ]`, where\u00A0`salt`\u00A0is the result of calling\u00A0`[interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).random()`, and\u00A0`commitment`\u00A0is the\u00A0[digest](https:\/\/docs.reach.sh\/ref-model.html#%28tech._digest%29)\u00A0of\u00A0`salt`\u00A0and\u00A0`x`.";
	} else if (word == 'commit') {
		buf = "### commit\r\n\r\ncommit();\r\n\r\nA\u00A0commit statement, written\u00A0`[commit](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28commit%29%29%29)();`,\u00A0[commits](https:\/\/docs.reach.sh\/ref-model.html#%28tech._commit%29)\u00A0to\u00A0[statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._statement%29)\'s\u00A0[continuation](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._continuation%29)\u00A0as the next\u00A0[step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._step%29)\u00A0of the\u00A0[DApp](https:\/\/docs.reach.sh\/ref-model.html#%28tech._dapp%29)\u00A0computation. In other words, it ends the current\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29)\u00A0and allows more\u00A0[local steps](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29).";
	} else if (word == 'Participant.set' || word == 'set') {
		buf = "### Participant.set\u00A0and\u00A0.set\r\n\r\nParticipant.[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)(PART, ADDR);\r\nPART.[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)(ADDR);\r\n\r\nAssigns the given\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0to the given address. If a\u00A0[backend](https:\/\/docs.reach.sh\/ref-model.html#%28tech._backend%29)\u00A0is running for this\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0and its address does not match the given address, then it will abort. This may only occur within a\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29).";
	} else if (word == 'while') {
		buf = "### while\r\n\r\nvar [ heap1, heap2 ] = [ 21, 21 ];\r\ninvariant([balance](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28balance%29%29%29)() [==](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d~3d%29%29%29) 2 [*](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2A%29%29%29) wagerAmount);\r\nwhile ( heap1 [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) heap2 [>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3e%29%29%29) 0 ) {\r\n  ....\r\n  [ heap1, heap2 ] = [ heap1 [-](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28-%29%29%29) 1, heap2 ];\r\n  [continue](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28continue%29%29%29); }\r\n\r\nA\u00A0while statement\u00A0may occur within a\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29)\u00A0and is written:\r\n\r\n[var](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28var%29%29%29) LHS = INIT_EXPR;\r\n[invariant](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28invariant%29%29%29)(INVARIANT_EXPR);\r\n[while](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28while%29%29%29)( COND_EXPR ) BLOCK\r\n\r\nwhere\u00A0`LHS`\u00A0is a valid left-hand side of an\u00A0[identifier definition](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._identifier._definition%29)\u00A0where the\u00A0[expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0`INIT_EXPR`\u00A0is the right-hand side, and\u00A0`INVARIANT_EXPR`\u00A0is an\u00A0[expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29), called the\u00A0loop invariant, that must be true before and after every execution of the\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29)\u00A0`BLOCK`, and if\u00A0`COND_EXPR`\u00A0is true, then the\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29)\u00A0executes, and if not, then the loop terminates and control transfers to the\u00A0[continuation](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._continuation%29)\u00A0of the\u00A0[while statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._while._statement%29). The identifiers bound by\u00A0`LHS`\u00A0are bound within\u00A0`INVARIANT_EXPR`,\u00A0`COND_EXPR`,\u00A0`BLOCK`, and the\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29)\u00A0of the\u00A0[while statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._while._statement%29).";
	} else if (word == 'continue') {
		buf = "### continue\r\n\r\n[ heap1, heap2 ] = [ heap1 [-](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28-%29%29%29) 1, heap2 ];\r\ncontinue;\r\n\r\nA\u00A0continue statement\u00A0may occur within a\u00A0[while statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._while._statement%29)\'s\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29)\u00A0and is written:\r\n\r\nLHS = UPDATE_EXPR;\r\n[continue](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28continue%29%29%29);\r\n\r\nwhere the identifiers bound by\u00A0`LHS`\u00A0are a subset of the variables bound by the nearest enclosing\u00A0[while statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._while._statement%29)\u00A0and\u00A0`UPDATE_EXPR`\u00A0is an\u00A0[expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0which may be bound by\u00A0`LHS`.\r\n\r\nA\u00A0[continue statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._continue._statement%29)\u00A0is a\u00A0[terminator statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._terminator._statement%29), so it must have an empty\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29).\r\n\r\nA\u00A0[continue statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._continue._statement%29)\u00A0may be written without the preceding identifier update, which is equivalent to writing\r\n\r\n[] = [];\r\n[continue](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28continue%29%29%29);\r\n\r\nA\u00A0[continue statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._continue._statement%29)\u00A0must be dominated by a\u00A0[consensus transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._transfer%29), which means that the body of a\u00A0[while statement](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._while._statement%29)\u00A0must always\u00A0`[commit](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28commit%29%29%29)();`\u00A0before calling\u00A0`[continue](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28continue%29%29%29);`. This restriction may be lifted in future versions of Reach, which will perform termination checking.";
	} else if (word == 'transfer') {
		buf = "### transfer\r\n\r\ntransfer(10).to(Alice)\r\n\r\nA\u00A0transfer expression, written\u00A0`[transfer](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28transfer%29%29%29)(AMOUNT_EXPR).to(PART)`, where\u00A0`AMOUNT_EXPR`\u00A0is an\u00A0[expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0that evaluates to a natural number and\u00A0`PART`\u00A0is a\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0identifier, performs a\u00A0[transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._transfer%29)\u00A0of\u00A0[network tokens](https:\/\/docs.reach.sh\/ref-model.html#%28tech._network._token%29)\u00A0from the\u00A0[contract](https:\/\/docs.reach.sh\/ref-model.html#%28tech._contract%29)\u00A0to the named\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29).\u00A0`AMOUNT_EXPR`\u00A0must evaluate to less than or equal to the balance of\u00A0[network tokens](https:\/\/docs.reach.sh\/ref-model.html#%28tech._network._token%29)\u00A0in the\u00A0[contract](https:\/\/docs.reach.sh\/ref-model.html#%28tech._contract%29)\u00A0[account](https:\/\/docs.reach.sh\/ref-model.html#%28tech._account%29). A\u00A0[transfer expression](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28tech._transfer._expression%29)\u00A0may only occur within a\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29).";
	} else if (word == 'require') {
		buf = "### require\r\n\r\nrequire( claim, [msg] )\r\n\r\nAn\u00A0[requirement](https:\/\/docs.reach.sh\/ref-model.html#%28tech._requirement%29)\u00A0where\u00A0`claim`\u00A0evaluates to\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0with\u00A0[honest](https:\/\/docs.reach.sh\/ref-model.html#%28tech._honest%29)\u00A0[participants](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29). This may only appear in a\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29). It accepts an optional bytes argument, which is included in any reported violation.";
	} else if (word == 'checkCommitment') {
		buf = "### checkCommitment\r\n\r\ncheckCommitment( commitment, salt, x )\r\n\r\nMakes a\u00A0[requirement](https:\/\/docs.reach.sh\/ref-model.html#%28tech._requirement%29)\u00A0that\u00A0`commitment`\u00A0is the\u00A0[digest](https:\/\/docs.reach.sh\/ref-model.html#%28tech._digest%29)\u00A0of\u00A0`salt`\u00A0and\u00A0`x`.";
	} else if (word == 'const' || word == 'function') {
		buf = "### const\u00A0and\u00A0function\r\n\r\nAn\u00A0identifier definition\u00A0is either a\u00A0[value definition](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._value._definition%29)\u00A0or a\u00A0[function definition](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._function._definition%29). Each of these introduces one or more\u00A0bound identifiers.\r\n\r\n---\r\n\r\nconst DELAY = 10;\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) [ Good, Bad ] = [ 42, 43 ];\r\n\r\n> > > [Valid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._valid%29)\u00A0identifiers\u00A0follow the same rules as JavaScript identifiers: they may consist of Unicode alphanumeric characters, or\u00A0`_`\u00A0or\u00A0`$`, but may not begin with a digit.\r\n\r\nA\u00A0value definition\u00A0is written\u00A0`[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) LHS = RHS;`.\r\n\r\n`LHS`\u00A0must obey the grammar:\r\n\r\n|  |\r\n\r\n\u2039LHS\u203A\r\n\r\n |\r\n\r\n\u00A0::=\u00A0\r\n\r\n |\r\n\r\n\u2039id\u203A\r\n\r\n |\r\n|  |  |\r\n\r\n\u00A0\u00A0|\u00A0\u00A0\r\n\r\n |\r\n\r\n[\u00A0\u2039LHS-tuple-seq\u203A\u00A0]\r\n\r\n |\r\n|  |  |\r\n\r\n\u00A0\u00A0|\u00A0\u00A0\r\n\r\n |\r\n\r\n{\u00A0\u2039LHS-obj-seq\u203A\u00A0}\r\n\r\n |\r\n|  |\r\n\r\n\u2039LHS-tuple-seq\u203A\r\n\r\n |\r\n\r\n\u00A0::=\u00A0\r\n\r\n |  |\r\n|  |  |\r\n\r\n\u00A0\u00A0|\u00A0\u00A0\r\n\r\n |\r\n\r\n...\u00A0\u2039LHS\u203A\r\n\r\n |\r\n|  |  |\r\n\r\n\u00A0\u00A0|\u00A0\u00A0\r\n\r\n |\r\n\r\n\u2039LHS\u203A\r\n\r\n |\r\n|  |  |\r\n\r\n\u00A0\u00A0|\u00A0\u00A0\r\n\r\n |\r\n\r\n\u2039LHS\u203A\u00A0,\u00A0\u2039LHS-tuple-seq\u203A\r\n\r\n |\r\n|  |\r\n\r\n\u2039LHS-obj-seq\u203A\r\n\r\n |\r\n\r\n\u00A0::=\u00A0\r\n\r\n |  |\r\n|  |  |\r\n\r\n\u00A0\u00A0|\u00A0\u00A0\r\n\r\n |\r\n\r\n...\u00A0\u2039LHS\u203A\r\n\r\n |\r\n|  |  |\r\n\r\n\u00A0\u00A0|\u00A0\u00A0\r\n\r\n |\r\n\r\n\u2039id\u203A\r\n\r\n |\r\n|  |  |\r\n\r\n\u00A0\u00A0|\u00A0\u00A0\r\n\r\n |\r\n\r\n\u2039id\u203A\u00A0,\u00A0\u2039LHS-obj-seq\u203A\r\n\r\n |\r\n\r\n`RHS`\u00A0must be compatible with the given\u00A0`LHS`. That is, if a\u00A0`LHS`\u00A0is an\u00A0\u2039LHS-tuple-seq\u203A, then the corresponding\u00A0`RHS`\u00A0must be a tuple with the correct number of elements. If a\u00A0`LHS`\u00A0is an\u00A0\u2039LHS-obj-seq\u203A, then the corresponding\u00A0`RHS`\u00A0must be an object with the correct fields.\r\n\r\nThose\u00A0[values](https:\/\/docs.reach.sh\/ref-model.html#%28tech._value%29)\u00A0are available as their corresponding\u00A0[bound identifier](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._bound._identifier%29)s in the statement\'s\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29).\r\n\r\n---\r\n\r\nfunction randomBool() {\r\n  [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) ([interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).random() [%](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~25%29%29%29) 2) [==](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d~3d%29%29%29) 0; };\r\n\r\nA\u00A0function definition, written\u00A0`[function](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28function%29%29%29) FUN(ARG_0, ..., ARG_n) BLOCK;`, defines\u00A0`FUN`\u00A0as a function which abstracts its\u00A0function body, the\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29)\u00A0`BLOCK`, over the identifiers\u00A0`ARG_0`\u00A0through\u00A0`ARG_n`.\r\n\r\n---\r\n\r\nAll identifiers in Reach programs must be\u00A0unbound\u00A0at the position of the program where they are bound, i.e., it is\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29)\u00A0to shadow identifiers with new definitions. For example,\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 3;\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 4;\r\n\r\nis\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29). This restriction is independent of whether a binding is only known to a single\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29). For example,\r\n\r\nAlice.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 3; });\r\nBob.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 3; });\r\n\r\nis\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29).\r\n\r\nThe special identifier\u00A0`_`\u00A0is an exception to this rule. The\u00A0`_`\u00A0binding is always considered to be unbound. This means means that\u00A0`_`\u00A0is both an identifier that can never be read, as well as an identifier that may be bound many times. This may be useful for ignoring unwanted values, for example:\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) [_, x, _] = [1, 2, 3];";
	} else if (word == 'return') {
		buf = "### return\r\n\r\nreturn 17;\r\n[return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) 3 [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 4;\r\n[return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) f(2, [false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29));\r\n[return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29);\r\n\r\nA\u00A0return statement, written\u00A0`[return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) EXPR;`, where\u00A0`EXPR`\u00A0is an\u00A0[expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0evaluates to the same\u00A0[value](https:\/\/docs.reach.sh\/ref-model.html#%28tech._value%29)\u00A0as\u00A0`EXPR`. As a special case,\u00A0`[return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29);`\u00A0is interpreted the same as\u00A0`[return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) [null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28null%29%29%29);`.\r\n\r\nA\u00A0[return statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._return._statement%29)\u00A0returns its value to the surrounding function application.\r\n\r\nA\u00A0[return statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._return._statement%29)\u00A0is a\u00A0[terminator statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._terminator._statement%29), so it must have an empty\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29). For example,\r\n\r\n{ [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) 1;\r\n  [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) 2; }\r\n\r\nis\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29), because the first\u00A0`[return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29)`\'s\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29)\u00A0is not empty.";
	} else if (word == 'if') {
		buf = "### if\r\n\r\nif ( 1 [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 2 [<](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3c%29%29%29) 3 ) {\r\n  [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) \"Yes!\";\r\n} else {\r\n  [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) \"No, waaah!\"; }\r\n\r\nA\u00A0conditional statement, written\u00A0`[if](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28if%29%29%29) (COND) NOT_FALSE [else](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28else%29%29%29) FALSE`, where\u00A0`COND`\u00A0is an\u00A0[expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._expression%29)\u00A0and\u00A0`NOT_FALSE`\u00A0and\u00A0`FALSE`\u00A0as\u00A0[statements](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._statement%29)\u00A0(potentially\u00A0[block statements](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block._statement%29)), selects between the\u00A0`NOT_FALSE`\u00A0[statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._statement%29)\u00A0and\u00A0`FALSE`\u00A0[statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._statement%29)\u00A0based on whether\u00A0`COND`\u00A0evaluates to\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`.\r\n\r\nBoth\u00A0`NOT_FALSE`\u00A0and\u00A0`FALSE`\u00A0have empty\u00A0[tails](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29), i.e. the\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29)\u00A0of the\u00A0[conditional statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._conditional._statement%29)\u00A0is not propagated. For example,\r\n\r\n[if](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28if%29%29%29) ( x [<](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3c%29%29%29) y ) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) z = 3; }\r\n[else](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28else%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) z = 4; }\r\n[return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) z;\r\n\r\nis erroneous, because the identifier\u00A0`z`\u00A0is not bound outside the\u00A0[conditional statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._conditional._statement%29).\r\n\r\nA\u00A0[conditional statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._conditional._statement%29)\u00A0may only include a\u00A0[consensus transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._transfer%29)\u00A0in\u00A0`NOT_FALSE`\u00A0or\u00A0`FALSE`\u00A0if it is within a\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29), because its statements are in the same context as the conditional statement itself.";
	} else if (word == 'switch') {
		buf = "### switch\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) mi = [Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29)([UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)).[Some](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Some%29%29%29)(42);\r\nswitch ( mi ) {\r\n case [None](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.None%29%29%29): [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) 8;\r\n [case](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28case%29%29%29) [Some](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Some%29%29%29): [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) mi [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 10; }\r\n[switch](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28switch%29%29%29) ( mi ) {\r\n [case](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28case%29%29%29) [None](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.None%29%29%29): [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) 8;\r\n default: [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) 41; }\r\n\r\nA\u00A0switch statement, written\u00A0`[switch](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28switch%29%29%29) (VAR) { CASE ... }`, where\u00A0`VAR`\u00A0is a variable bound to a\u00A0[data instance](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._data._instance%29)\u00A0and\u00A0`CASE`\u00A0is either\u00A0`[case](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28case%29%29%29) VARIANT: STMT ...`, where\u00A0`VARIANT`\u00A0is a variant, or\u00A0`[default](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28default%29%29%29): STMT ...`,\u00A0`STMT`\u00A0is a sequence of statements, selects the appropriate sequence of statements based on which variant\u00A0`VAR`\u00A0holds. Within the body of a\u00A0`[switch](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28switch%29%29%29)`\u00A0case,\u00A0`VAR`\u00A0has the type of variant; i.e. in a\u00A0`[Some](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Some%29%29%29)`\u00A0case of a\u00A0`[Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29)([UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29))`\u00A0`[switch](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28switch%29%29%29)`, the variable is bound to an integer.\r\n\r\nAll cases have empty\u00A0[tails](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29), i.e. the\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29)\u00A0of the\u00A0[switch statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._switch._statement%29)\u00A0is not propagated.\r\n\r\nA\u00A0[switch statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._switch._statement%29)\u00A0may only include a\u00A0[consensus transfer](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._transfer%29)\u00A0in its cases if it is within a\u00A0[consensus step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._step%29), because its statements are in the same context as the conditional statement itself.\r\n\r\nIt is\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29)\u00A0for a case to appear multiple times, or be missing, or to be superfluous (i.e. for a variant that does not exist in the\u00A0`[Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Data%29%29%29)`\u00A0type of\u00A0`VAR`).";
	} else if (word == 'array') {
		buf = "### array\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = array([1, 2, 3]);\r\n\r\nConverts a\u00A0[tuple](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tuple%29)\u00A0of homogenueous values into an\u00A0array.";
	} else if (word == 'Tuple.length' || word == 'Array.length' || word == 'length') {
		buf = "### Array & tuple length:\u00A0Tuple.length,\u00A0Array.length, and\u00A0.length\r\n\r\n[Tuple](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Tuple%29%29%29).length(tup);\r\ntup.[length](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28length%29%29%29);\r\n[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[length](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28length%29%29%29)(arr);\r\narr.[length](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28length%29%29%29);\r\n\r\n`[Tuple](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Tuple%29%29%29).[length](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28length%29%29%29)`\u00A0Returns the length of the given tuple.\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[length](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28length%29%29%29)`\u00A0Returns the length of the given array.\r\n\r\nBoth may be abbreviated as\u00A0`expr.[length](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28length%29%29%29)`\u00A0where\u00A0`expr`\u00A0evaluates to a tuple or an array.";
	} else if (word == 'Tuple.set' || word == 'Array.set' || word == 'set') {
		buf = "### Array & tuple update:\u00A0Tuple.set,\u00A0Array.set, and\u00A0.set\r\n\r\n[Tuple](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Tuple%29%29%29).set(tup, idx, val);\r\ntup.[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)(idx, val);\r\n[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)(arr, idx, val);\r\narr.[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)(idx, val);\r\n\r\n`[Tuple](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Tuple%29%29%29).[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)`\u00A0Returns a new tuple identical to\u00A0`tup`, except that index\u00A0`idx`\u00A0is replaced with\u00A0`val`.\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)`\u00A0Returns a new array identical to\u00A0`arr`, except that index\u00A0`idx`\u00A0is replaced with\u00A0`val`.\r\n\r\nBoth may be abbreviated as\u00A0`expr.[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)(idx, val)`\u00A0where\u00A0`expr`\u00A0evaluates to a tuple or an array.";
	} else if (word == 'Array.iota' || word == 'iota') {
		buf = "### Array.iota\r\n\r\n        Array.iota(5)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[iota](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28iota%29%29%29)(len)`\u00A0returns an array of length\u00A0`len`, where each element is the same as its index. For example,\u00A0`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[iota](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28iota%29%29%29)(4)`\u00A0returns\u00A0`[0, 1, 2, 3]`. The given\u00A0`len`\u00A0must evaluate to an integer at compile-time.";
	} else if (word == 'Array.replicate' || word == 'replicate') {
		buf = "### Array.replicate\u00A0&&\u00A0.replicate\r\n\r\n        Array.replicate(5, \"five\")\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).replicate(len, val)`\u00A0returns an array of length\u00A0`len`, where each element is\u00A0`val`. For example,\u00A0`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).replicate(4, \"four\")`\u00A0returns\u00A0`[\"four\", \"four\", \"four\", \"four\"]`. The given\u00A0`len`\u00A0must evaluate to an integer at compile-time.";
	} else if (word == 'Array.concat' || word == 'concat') {
		buf = "### Array.concat\u00A0&&\u00A0.concat\r\n\r\n        Array.concat(x, y)\r\n        x.concat(y)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[concat](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28concat%29%29%29)(x, y)`\u00A0concatenates the two arrays\u00A0`x`\u00A0and\u00A0`y`. This may be abbreviated as\u00A0`x.[concat](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28concat%29%29%29)(y)`.";
	} else if (word == 'Array.empty' || word == 'empty') {
		buf = "### Array.empty\r\n\r\n        Array_empty\r\n        Array.empty\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).empty`\u00A0is an array with no elements. It is the identity element of\u00A0`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[concat](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28concat%29%29%29)`. It may also be written\u00A0`Array_empty`.";
	} else if (word == 'Array.zip' || word == 'zip') {
		buf = "### Array.zip\u00A0&&\u00A0.zip\r\n\r\n        Array.zip(x, y)\r\n        x.zip(y)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[zip](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28zip%29%29%29)(x, y)`\u00A0returns a new array the same size as\u00A0`x`\u00A0and\u00A0`y`\u00A0(which must be the same size) whose elements are tuples of the elements of\u00A0`x`\u00A0and\u00A0`y`. This may be abbreviated as\u00A0`x.[zip](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28zip%29%29%29)(y)`.";
	} else if (word == 'Array.map' || word == 'map') {
		buf = "### Array.map\u00A0&&\u00A0.map\r\n\r\n        Array.map(arr, f)\r\n        arr.map(f)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[map](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28map%29%29%29)(arr, f)`\u00A0returns a new array,\u00A0`arr_mapped`, the same size as\u00A0`arr`, where\u00A0`arr_mapped[i] = f(arr[i])`\u00A0for all\u00A0`i`. For example,\u00A0`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[iota](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28iota%29%29%29)(4).[map](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28map%29%29%29)(x [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) x[+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29)1)`\u00A0returns\u00A0`[1, 2, 3, 4]`. This may be abbreviated as\u00A0`arr.[map](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28map%29%29%29)(f)`.\r\n\r\nThis function is generalized to an arbitrary number of arrays of the same size, which are provided before the\u00A0`f`\u00A0argument. For example,\u00A0`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[iota](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28iota%29%29%29)(4).[map](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28map%29%29%29)([Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[iota](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28iota%29%29%29)(4), [add](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28add%29%29%29))`\u00A0returns\u00A0`[0, 2, 4, 6]`.";
	} else if (word == 'Array.reduce' || word == 'reduce') {
		buf = "### Array.reduce\u00A0&&\u00A0.reduce\r\n\r\n        Array.reduce(arr, z, f)\r\n        arr.reduce(z, f)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[reduce](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28reduce%29%29%29)(arr, z, f)`\u00A0returns the\u00A0[left fold](https:\/\/en.wikipedia.org\/wiki\/Fold_(higher-order_function))\u00A0of the function\u00A0`f`\u00A0over the given array with the initial value\u00A0`z`. For example,\u00A0`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[iota](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28iota%29%29%29)(4).[reduce](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28reduce%29%29%29)(0, [add](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28add%29%29%29))`\u00A0returns\u00A0`((0 [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 1) [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 2) [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 3 = 6`. This may be abbreviated as\u00A0`arr.[reduce](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28reduce%29%29%29)(z, f)`.\r\n\r\nThis function is generalized to an arbitrary number of arrays of the same size, which are provided before the\u00A0`z`\u00A0argument. For example,\u00A0\r\n\r\n        Array.iota(4)\r\n          .reduce(Array.iota(4), 0, (x, y, z) =>\r\n            (z + x + y))\r\n\r\nreturns:\u00A0\r\n\r\n        ((((0 + 0 + 0) + 1 + 1) + 2 + 2) + 3 + 3)";
	} else if (word == 'Array.forEach' || word == 'forEach') {
		buf = "### Array.forEach\u00A0&&\u00A0.forEach\r\n\r\n        arr.forEach(f)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[forEach](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28for.Each%29%29%29)(arr, f)`\u00A0iterates the function\u00A0`f`\u00A0over the elements of the array\u00A0`arr`, discarding the result. This may be abbreviated as\u00A0`arr.[forEach](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28for.Each%29%29%29)(f)`.";
	} else if (word == 'Array.all' || word == 'all') {
		buf = "### Array.all\u00A0&&\u00A0.all\r\n\r\n        Array.all(arr, f)\r\n        arr.all(f)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[all](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28all%29%29%29)(arr, f)`\u00A0determines whether the predicate,\u00A0f, is satisfied by every element of the array,\u00A0arr.";
	} else if (word == 'Array.any' || word == 'any') {
		buf = "### Array.any\u00A0&&\u00A0.any\r\n\r\n        Array.any(arr, f)\r\n        arr.any(f)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[any](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28any%29%29%29)(arr, f)`\u00A0determines whether the predicate,\u00A0f, is satisfied by at least one element of the array,\u00A0arr.";
	} else if (word == 'Array.or' || word == 'or') {
		buf = "### Array.or\u00A0&&\u00A0.or\r\n\r\n        Array.or(arr)\r\n        arr.or()\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[or](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28or%29%29%29)(arr)`\u00A0returns the disjunction of an array of\u00A0`[Bool](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Bool%29%29%29)`s.";
	} else if (word == 'Array.and' || word == 'and') {
		buf = "### Array.and\u00A0&&\u00A0.and\r\n\r\n        Array.and(arr)\r\n        arr.and()\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[and](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28and%29%29%29)(arr)`\u00A0returns the conjunction of an array of\u00A0`[Bool](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Bool%29%29%29)`s.";
	} else if (word == 'Array.includes' || word == 'includes') {
		buf = "### Array.includes\u00A0&&\u00A0.includes\r\n\r\n        Array.includes(arr, x)\r\n        arr.includes(x)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[includes](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28includes%29%29%29)(arr, x)`\u00A0determines whether the array includes the element,\u00A0x.";
	} else if (word == 'Array.indexOf' || word == 'indexOf') {
		buf = "### Array.indexOf\u00A0&&\u00A0.indexOf\r\n\r\n        Array.indexOf(arr, x)\r\n        arr.indexOf(x)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[indexOf](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28index.Of%29%29%29)(arr, x)`\u00A0returns the index of the first element in the given array that is equal to\u00A0x. The return value is of type\u00A0`[Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29)([UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29))`. If the value is not present in the array,\u00A0`[None](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.None%29%29%29)`\u00A0is returned.";
	} else if (word == 'Array.findIndex' || word == 'findIndex') {
		buf = "### Array.findIndex\u00A0&&\u00A0.findIndex\r\n\r\n        Array.findIndex(arr, f)\r\n        arr.findIndex(f)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[findIndex](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28find.Index%29%29%29)(arr, f)`\u00A0returns the index of the first element in the given array that satisfies the predicate\u00A0f. The return value is of type\u00A0`[Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29)([UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29))`. If no value in the array satisfies the predicate,\u00A0`[None](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.None%29%29%29)`\u00A0is returned.";
	} else if (word == 'Array.count' || word == 'count') {
		buf = "### Array.count\u00A0&&\u00A0.count\r\n\r\n        Array.count(arr, f)\r\n        arr.count(f)\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[count](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28count%29%29%29)(arr, f)`\u00A0returns the number of elements in\u00A0arr\u00A0that satisfy the predicate,\u00A0f.";
	} else if (word == 'Array.min' || word == 'min') {
		buf = "### Array.min\u00A0&&\u00A0.min\r\n\r\n        Array.min(arr)\r\n        arr.min()\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[min](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28min%29%29%29)(arr)`\u00A0returns the lowest number in an array of\u00A0UInts.";
	} else if (word == 'Array.max' || word == 'max') {
		buf = "### Array.max\u00A0&&\u00A0.max\r\n\r\n        Array.max(arr)\r\n        arr.max()\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[max](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28max%29%29%29)(arr)`\u00A0returns the largest number in an array of\u00A0UInts.";
	} else if (word == 'Array.sum' || word == 'sum') {
		buf = "### Array.sum\u00A0&&\u00A0.sum\r\n\r\n        Array.sum(arr)\r\n        arr.sum()\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[sum](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28sum%29%29%29)(arr)`\u00A0returns the sum of an array of\u00A0UInts.";
	} else if (word == 'Array.product' || word == 'product') {
		buf = "### Array.product\u00A0&&\u00A0.product\r\n\r\n        Array.product(arr)\r\n        arr.product()\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[product](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28product%29%29%29)(arr)`\u00A0returns the product of an array of\u00A0UInts.";
	} else if (word == 'Array.average' || word == 'average') {
		buf = "### Array.average\u00A0&&\u00A0.average\r\n\r\n        Array.average(arr)\r\n        arr.average()\r\n\r\n`[Array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Array%29%29%29).[average](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28average%29%29%29)(arr)`\u00A0returns the mean of an array of\u00A0UInts.";
	} else if (word == 'Object.set') {
		buf = "### Object.set\r\n\r\n[Object](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Object%29%29%29).[set](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28set%29%29%29)(obj, fld, val);\r\nObject_set(obj, fld, val);\r\n{ ...obj, [fld]: val };\r\n\r\nReturns a new object identical to\u00A0`obj`, except that field\u00A0`fld`\u00A0is replaced with\u00A0`val`.";
	} else if (word == 'Data') {
		buf = "### Data\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) Taste = [Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Data%29%29%29)({Salty: [Null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Null%29%29%29),\r\n                    Spicy: [Null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Null%29%29%29),\r\n                    Sweet: [Null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Null%29%29%29),\r\n                    Umami: [Null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Null%29%29%29)});\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) burger = Taste.Umami();\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) Shape = [Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Data%29%29%29)({ Circle: [Object](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Object%29%29%29)({r: [UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)}),\r\n                     Square: [Object](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Object%29%29%29)({s: [UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)}),\r\n                     Rect: [Object](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Object%29%29%29)({w: [UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29), h: [UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)}) });\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) nice = Shape.Circle({r: 5});\r\n\r\nA\u00A0data instance\u00A0is written\u00A0`DATA.VARIANT(VALUE)`, where\u00A0`DATA`\u00A0is\u00A0`[Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Data%29%29%29)`\u00A0type,\u00A0`VARIANT`\u00A0is the name of one of\u00A0`DATA`\'s variants, and\u00A0`VALUE`\u00A0is a value matching the type of the variant. As a special case, when the type of a variant is\u00A0`[Null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Null%29%29%29)`, the\u00A0`VALUE`\u00A0may be omitted, as shown in the definition of\u00A0`burger`\u00A0in the same above.\r\n\r\n[Data instances](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._data._instance%29)\u00A0are consumed by\u00A0`[switch](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28switch%29%29%29)`\u00A0statements.";
	} else if (word == 'Maybe') {
		buf = "### Maybe\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) MayInt = Maybe([UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29));\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) bidA = MayInt.Some(42);\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) bidB = MayInt.None([null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28null%29%29%29));\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) getBid = (m) [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) fromMaybe(m, (() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) 0), ((x) [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) x));\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) bidSum = getBid(bidA) [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) getBid(bidB);\r\n[assert](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28assert%29%29%29)(bidSum [==](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d~3d%29%29%29) 42);\r\n\r\n[Option types](https:\/\/en.wikipedia.org\/wiki\/Option_type)\u00A0are represented in Reach through the built-in\u00A0`[Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Data%29%29%29)`\u00A0type,\u00A0`[Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29)`, which has two variants:\u00A0`[Some](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Some%29%29%29)`\u00A0and\u00A0`[None](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.None%29%29%29)`.\r\n\r\n`[Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29)`\u00A0is defined by\r\n\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) [Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29) = (A) [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) [Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Data%29%29%29)({[None](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.None%29%29%29): [Null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Null%29%29%29), [Some](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Some%29%29%29): A});\r\n\r\nThis means it is a function that returns a\u00A0`[Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Data%29%29%29)`\u00A0type specialized to a particular type in the\u00A0`[Some](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Some%29%29%29)`\u00A0variant.\r\n\r\n`[Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29)`\u00A0instances can be conveniently consumed by\u00A0`[fromMaybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28from.Maybe%29%29%29)(mValue, onNone, onSome)`, where\u00A0`onNone`\u00A0is a function of no arguments which is called when\u00A0`mValue`\u00A0is\u00A0`[None](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.None%29%29%29)`,\u00A0`onSome`\u00A0is a function of on argument which is called with the value when\u00A0`mValue`\u00A0is\u00A0`[Some](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Some%29%29%29)`, and\u00A0`mValue`\u00A0is a\u00A0[data instance](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._data._instance%29)\u00A0of\u00A0`[Maybe](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Maybe%29%29%29)`.";
	} else if (word == 'makeEnum') {
		buf = "### makeEnum\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);\r\n\r\nAn\u00A0enumeration\u00A0(or\u00A0enum, for short), can be created by calling the\u00A0`[makeEnum](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28make.Enum%29%29%29)`\u00A0function, as in\u00A0`[makeEnum](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28make.Enum%29%29%29)(N)`, where\u00A0`N`\u00A0is the number of distinct values in the enum. This produces a tuple of\u00A0`N[+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29)1`\u00A0values, where the first value is a\u00A0`[Fun](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Fun%29%29%29)([[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)], [Bool](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Bool%29%29%29))`\u00A0which tells you if its argument is one of the enum\'s values, and the next N values are distinct\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)`s.";
	} else if (word == 'assert') {
		buf = "### assert\r\n\r\nassert( claim, [msg] )\r\n\r\nA\u00A0[static assertion](https:\/\/docs.reach.sh\/ref-model.html#%28tech._static._assertion%29)\u00A0which is only\u00A0[valid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._valid%29)\u00A0if\u00A0`claim`\u00A0always evaluates to\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`.\r\n\r\n> > > The Reach compiler will produce a counter-example (i.e. an assignment of the identifiers in the program to falsify the\u00A0`claim`) when an\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29)\u00A0`claim`\u00A0is provided. It is possible to write a\u00A0`claim`\u00A0that actually always evaluates to\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`, but for which our current approach cannot prove always evaluates to\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`; if this is the case, Reach will fail to compile the program, reporting that its analysis is incomplete. Reach will never produce an erroneous counter-example.\r\n\r\nIt accepts an optional bytes argument, which is included in any reported violation.";
	} else if (word == 'forall') {
		buf = "### forall\r\n\r\nforall( Type )\r\n[forall](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28forall%29%29%29)( Type, ([var](https:\/\/docs.reach.sh\/ref-programs-consensus.html#%28reach._%28%28var%29%29%29)) [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) BLOCK )\r\n\r\nThe single argument version returns an abstract value of the given type. It may only be referenced inside of\u00A0[assert](https:\/\/docs.reach.sh\/ref-model.html#%28tech._assert%29)ions; any other reference is invalid.\r\n\r\nThe two argument version is an abbreviation of calling the second argument with the result of\u00A0`[forall](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28forall%29%29%29)(Type)`. This is convenient for writing general claims about expressions, such as\r\n\r\n[forall](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28forall%29%29%29)([UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29), (x) [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) [assert](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28assert%29%29%29)(x [==](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d~3d%29%29%29) x));";
	} else if (word == 'possible') {
		buf = "### possible\r\n\r\npossible( claim, [msg] )\r\n\r\nA\u00A0[possibility assertion](https:\/\/docs.reach.sh\/ref-model.html#%28tech._possibility._assertion%29)\u00A0which is only\u00A0[valid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._valid%29)\u00A0if it is possible for\u00A0`claim`\u00A0to evaluate to\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0with\u00A0[honest](https:\/\/docs.reach.sh\/ref-model.html#%28tech._honest%29)\u00A0[frontends](https:\/\/docs.reach.sh\/ref-model.html#%28tech._frontend%29)\u00A0and\u00A0[participants](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29). It accepts an optional bytes argument, which is included in any reported violation.";
	} else if (word == 'digest') {
		buf = "### digest\r\n\r\ndigest( arg_0, ..., arg_n )\r\n\r\nThe\u00A0[digest](https:\/\/docs.reach.sh\/ref-model.html#%28tech._digest%29)\u00A0primitive performs a\u00A0[cryptographic hash](https:\/\/en.wikipedia.org\/wiki\/Cryptographic_hash_function)\u00A0of the binary encoding of the given arguments. This returns a\u00A0`[Digest](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Digest%29%29%29)`\u00A0value. The exact algorithm used depends on the\u00A0[connector](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29).";
	} else if (word == 'balance') {
		buf = "### balance\r\n\r\nbalance()\r\n\r\nThe\u00A0balance\u00A0primitive returns the balance of the\u00A0[contract](https:\/\/docs.reach.sh\/ref-model.html#%28tech._contract%29)\u00A0[account](https:\/\/docs.reach.sh\/ref-model.html#%28tech._account%29)\u00A0for the\u00A0[DApp](https:\/\/docs.reach.sh\/ref-model.html#%28tech._dapp%29).";
	} else if (word == 'implies') {
		buf = "### implies\r\n\r\nimplies( x, y )\r\n\r\nReturns\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0if\u00A0`x`\u00A0is\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`\u00A0or\u00A0`y`\u00A0is\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`.";
	} else if (word == 'ensure') {
		buf = "### ensure\r\n\r\nensure( pred, x )\r\n\r\nMakes a\u00A0[static assertion](https:\/\/docs.reach.sh\/ref-model.html#%28tech._static._assertion%29)\u00A0that\u00A0`pred(x)`\u00A0is\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0and returns\u00A0`x`.";
	} else if (word == 'hasRandom') {
		buf = "### hasRandom\r\n\r\nhasRandom\r\n\r\nA\u00A0[participant interact interface](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._participant._interact._interface%29)\u00A0which specifies\u00A0random\u00A0as a function that takes no arguments are returns an unsigined integer of\u00A0bit width\u00A0bits.";
	} else if (word == 'Null' || word == 'Bool' || word == 'UInt' || word == 'Bytes' || word == 'Digest' || word == 'Address' || word == 'Fun' || word == 'Tuple' || word == 'Object' || word == 'Array' || word == 'Data') {
		buf = "### Types\r\n\r\nReach\'s\u00A0types are represented with programs by the following identifiers and constructors:\r\n\r\n-   `Null`.\r\n\r\n-   `Bool`, which denotes a boolean.\r\n\r\n-   `UInt`, which denotes an unsigned integer.\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29).max`\u00A0is the largest value that may be assigned to a\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)`.\r\n\r\n-   `Bytes`, which denotes a string of bytes.\r\n\r\n-   `Digest`, which denotes a\u00A0[digest](https:\/\/docs.reach.sh\/ref-model.html#%28tech._digest%29).\r\n\r\n-   `Address`, which denotes an\u00A0[account](https:\/\/docs.reach.sh\/ref-model.html#%28tech._account%29)\u00A0[address](https:\/\/docs.reach.sh\/ref-model.html#%28tech._addres%29).\r\n\r\n-   `Fun([Domain_0, ..., Domain_N], Range)`, which denotes a function type.\r\n\r\n-   `Tuple(Field_0, ..., FieldN)`, which denotes a tuple. (Refer to\u00A0[Tuples](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28part._ref-programs-tuples%29)\u00A0for constructing tuples.)\r\n\r\n-   `Object({key_0: Type_0, ..., key_N: Type_N})`, which denotes an object. (Refer to\u00A0[Objects](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28part._ref-programs-objects%29)\u00A0for constructing objects.)\r\n\r\n-   `Array(ElemenType, size)`, which denotes a statically-sized array. (Refer to\u00A0[array](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28part._ref-programs-arrays%29)\u00A0for constructing arrays.)\r\n\r\n-   `Data({variant_0: Type_0, ..., variant_N: Type_N})`, which denotes a\u00A0[tagged union](https:\/\/en.wikipedia.org\/wiki\/Tagged_union)\u00A0(or\u00A0sum type). (Refer to\u00A0[Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28part._ref-programs-data%29)\u00A0for constructing\u00A0[data instances](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._data._instance%29).)\r\n\r\n`[Object](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Object%29%29%29)`\u00A0and\u00A0`[Data](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Data%29%29%29)`\u00A0are commonly used to implemented\u00A0[algebraic data types](https:\/\/en.wikipedia.org\/wiki\/Algebraic_data_type)\u00A0in Reach.\r\n\r\ntypeOf(x)  \/\/ type\r\nisType(t) \/\/ Bool\r\n\r\nThe\u00A0`[typeOf](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28type.Of%29%29%29)`\u00A0primitive function is the same as\u00A0`[typeof](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28typeof%29%29%29)`: it returns the type of its argument. The\u00A0`[isType](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28is.Type%29%29%29)`\u00A0function returns\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0if its argument is a type. Any expression satisfying\u00A0`[isType](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28is.Type%29%29%29)`\u00A0is compiled away and does not exist at runtime.";
	} else if (word == 'verifyOverflow') {
		buf = "Determines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29).max`. This defaults to\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`, because it is onerous to verify. We recommend turning it on before final deployment, but leaving it off during development. When it is\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`,\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0will ensure that overflows do not actually occur on the network.";
	} else if (word == 'deployMode') {
		buf = "Determines whether\u00A0[contract](https:\/\/docs.reach.sh\/ref-model.html#%28tech._contract%29)\u00A0should be\u00A0[deploy](https:\/\/docs.reach.sh\/ref-model.html#%28tech._deploy%29)ed independently (`\'constructor\'`) or as part of the first\u00A0[publication](https:\/\/docs.reach.sh\/ref-model.html#%28tech._publication%29)\u00A0(`\'firstMsg\'`). If deployed as part of the first publication, then the first publication must precede all uses of\u00A0`[wait](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28wait%29%29%29)`\u00A0and\u00A0`.[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)`. See\u00A0[the guide on deployment modes](https:\/\/docs.reach.sh\/guide-deploymode.html)\u00A0for a discussion of why to choose a particular mode.";
	} else if (word == 'verifyPerConnector') {
		buf = "Determines whether verification is done per connector, or once for a generic connector. When this is\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`, then connector-specific constants, like\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29).max`, will be instantiated to literal numbers. This concretization of these constants can induce performance degradation in the verifier.";
	} else if (word == 'connectors') {
		buf = "A tuple of the\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0that the application should be compiled for. By default, all available\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0are chosen.";
	} else if (word == 'xor') {
		buf = "\t\txor(false, false); \/\/ false\r\n\t\txor(false, true);  \/\/ true\r\n\t\txor(true, false);  \/\/ true\r\n\t\txor(true, true);   \/\/ false \r\n\r\n`xor([Bool](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Bool%29%29%29), [Bool](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Bool%29%29%29))`\u00A0returns\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0only when the inputs differ in value.";
	} else if (word == 'FixedPoint') {
		buf = "`[FixedPoint](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Fixed.Point%29%29%29)`\u00A0is defined by\r\n\r\n\t\texport const FixedPoint = Object({ sign: bool, i: Object({ scale: UInt, i: UInt }) }); \r\n\r\n`[FixedPoint](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Fixed.Point%29%29%29)`\u00A0can be used to represent numbers with a fixed number of digits after the decimal point. They are handy for representing fractional values, especially in base 10. The value of a fixed point number is determined by dividing the underlying integer value,\u00A0i, by its scale factor,\u00A0scale. For example, we could represent the value\u00A0`1.234`\u00A0with\u00A0`{ sign: [Pos](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Pos%29%29%29), i: { scale: 1000, i : 1234 } }`\u00A0or\u00A0`[fx](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28fx%29%29%29)(1000)([Pos](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Pos%29%29%29), 1234)`. Alternatively, Reach provides syntactic sugar for defining\u00A0`[FixedPoint](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Fixed.Point%29%29%29)`\u00A0numbers. One can simply write\u00A0`1.234`, which will assume the value is in base 10. A scale factor of\u00A01000\u00A0correlates to 3 decimal places of precision. Similarly, a scale factor of\u00A0100\u00A0would have 2 decimal places of precision.";
	} else if (word == 'fx') {
		buf = "\t\tconst scale = 10;\r\n\t\tconst i = 56;\r\n\t\tfx(scale)(Neg, i); \/\/ represents - 5.6 \r\n\r\n`[fx](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28fx%29%29%29)(scale)(i)`\u00A0will return a function that can be used to instantiate fixed point numbers with a particular scale factor.\r\n";
	} else if (word == 'fxint') {
		buf = "\t\tconst i = 4;\r\n\t\tfxint(-i); \/\/ represents - 4.0 \r\n\r\n`[fxint](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28fxint%29%29%29)([Int](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Int%29%29%29))`\u00A0will cast the\u00A0`[Int](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Int%29%29%29)`\u00A0arg as a\u00A0`[FixedPoint](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Fixed.Point%29%29%29)`\u00A0number with a\u00A0scale\u00A0of 1.\r\n";
	} else if (word == 'fxrescale') {
		buf = "\t\tconst x = fx(1000)(Pos, 1234); \/\/ x = 1.234\r\n\t\tfxrescale(x, 100);    \/\/ => 1.23 \r\n\r\n`[fxrescale](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28fxrescale%29%29%29)(x, scale)`\u00A0will convert a fixed point number from using one scale to another. This operation can result in loss of precision, as demonstrated in the above example.\r\n";
	} else if (word == 'fxunify') {
		buf = "\t\tconst x = fx(1000)(Pos, 824345); \/\/ x = 824.345\r\n\t\tconst y = 45.67;\r\n\t\tfxunify(x, y);    \/\/ => [ 1000, 824.345, 45.670 ] \r\n\r\n`[fxunify](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28fxunify%29%29%29)(x, y)`\u00A0will convert the fixed point numbers to use the same scale. The larger scale of the two arguments will be chosen. The function will return a\u00A03-tuple\u00A0consisting of the common scale and the newly scaled values.\r\n";
	} else if (word == 'Int') {
		buf = "The standard library provides abstractions for dealing with signed integers. The following definitions are used to represent\u00A0`[Int](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Int%29%29%29)`s:\r\n\r\n\t\tconst Int = { sign: bool, i: UInt };\r\n\t\tconst Pos = true;\r\n\t\tconst Neg = false;\r\n\r\nInt\u00A0is represented as an object, as opposed to a scalar value, because some platforms that Reach targets do not provide native support for signed integers.\r\n"; }
	else if (word == 'Participant') {
		buf = "A\u00A0participant constructor\u00A0is used for declaring a logical actor in a Reach program. A\u00A0[participant](ref-model.html#%28tech._participant%29)\u00A0and\u00A0[participant class](ref-model.html#%28tech._participant._clas%29)\u00A0may be declared with\r\n\r\n    Participant(participantName, participantInteractInterface)\r\n\r\nand\r\n\r\n    ParticipantClass(participantName, participantInteractInterface)\r\n\r\nrespectively.\r\n\r\n`participantName`\u00A0is a string which indicates the name of the\u00A0[participant](ref-model.html#%28tech._participant%29)\u00A0function in the generated\u00A0[backend](ref-model.html#%28tech._backend%29)\u00A0code. Each\u00A0`participantName`\u00A0must be unique.\r\n\r\n`participantInteractInterface`\u00A0is a\u00A0participant interact interface, an object where each field indicates the type of a function or value which must be provided to the\u00A0[backend](ref-model.html#%28tech._backend%29)\u00A0by the\u00A0[frontend](ref-model.html#%28tech._frontend%29)\u00A0for\u00A0[interact](ref-model.html#%28tech._interact%29)ing with the\u00A0[participant](ref-model.html#%28tech._participant%29)."; }
	else if (word == 'ParticipantClass') {
		buf = "A\u00A0participant constructor\u00A0is used for declaring a logical actor in a Reach program. A\u00A0[participant](ref-model.html#%28tech._participant%29)\u00A0and\u00A0[participant class](ref-model.html#%28tech._participant._clas%29)\u00A0may be declared with\r\n\r\n    Participant(participantName, participantInteractInterface)\r\n\r\nand\r\n\r\n    ParticipantClass(participantName, participantInteractInterface)\r\n\r\nrespectively.\r\n\r\n`participantName`\u00A0is a string which indicates the name of the\u00A0[participant](ref-model.html#%28tech._participant%29)\u00A0function in the generated\u00A0[backend](ref-model.html#%28tech._backend%29)\u00A0code. Each\u00A0`participantName`\u00A0must be unique.\r\n\r\n`participantInteractInterface`\u00A0is a\u00A0participant interact interface, an object where each field indicates the type of a function or value which must be provided to the\u00A0[backend](ref-model.html#%28tech._backend%29)\u00A0by the\u00A0[frontend](ref-model.html#%28tech._frontend%29)\u00A0for\u00A0[interact](ref-model.html#%28tech._interact%29)ing with the\u00A0[participant](ref-model.html#%28tech._participant%29)."; }
	else if (word == 'timeRemaining') {
		buf = "When dealing with absolute deadlines in\u00A0`[parallelReduce](ref-programs-consensus.html#%28reach._%28%28parallelReduce%29%29%29)`, there is a common pattern in the\u00A0`TIMEOUT_BLOCK`\u00A0to have participants\u00A0`[race](ref-programs-step.html#%28reach._%28%28race%29%29%29)`\u00A0to\u00A0`[publish](ref-programs-step.html#%28reach._%28%28publish%29%29%29)`\u00A0and return the accumulator. There is a shorthand,\u00A0`.timeRemaining`, available for this situation:\r\n\r\n[const](#%28reach._%28%28const%29%29%29) [ timeRemaining, keepGoing ] = [makeDeadline](#%28reach._%28%28make.Deadline%29%29%29)(deadline);\r\n[const](#%28reach._%28%28const%29%29%29) [ x, y, z ] =\r\n  [parallelReduce](ref-programs-consensus.html#%28reach._%28%28parallelReduce%29%29%29)([ 1, 2, 3 ])\r\n    .[while](ref-programs-consensus.html#%28reach._%28%28while%29%29%29)(keepGoing())\r\n    ...\r\n    .timeRemaining(timeRemaining())\r\n\r\nwhich will expand to:\r\n\r\n.[timeout](ref-programs-step.html#%28reach._%28%28timeout%29%29%29)(timeRemaining(), () [=>](#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [race](ref-programs-step.html#%28reach._%28%28race%29%29%29)(...Participants).[publish](ref-programs-step.html#%28reach._%28%28publish%29%29%29)();\r\n  [return](#%28reach._%28%28return%29%29%29) [ x, y, z ]; })"; }
	else if (word == 'int') {
		buf = "\t\tint(Pos, 4); \/\/ represents 4\r\n\t\tint(Neg, 4); \/\/ represents -4\r\n\t\t-4;          \/\/ represents -4\r\n\t\t+4;          \/\/ represents 4 : Int\r\n\t\t4;          \/\/ represents 4 : UInt \r\n\r\n`[int](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28int%29%29%29)([Bool](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Bool%29%29%29), [UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29))`\u00A0is shorthand for defining an\u00A0`[Int](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Int%29%29%29)`\u00A0record. You may also use the\u00A0`[+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29)`\u00A0and\u00A0`[-](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28-%29%29%29)`\u00A0unary operators to declare integers instead of\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)`s.\r\n"; }
	else if (word == 'pow') {
		buf = "\t\tpow (2, 40, 10) \/\/ => 1,099,511,627,776 \r\n\r\n`[pow](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28pow%29%29%29)(base, power, precision)`\u00A0Calculates the approximate value of raising base to power. The third argument must be an\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29)`\u00A0whose value is known at compile time, which represents the number of iterations the algorithm should perform. For reference,\u00A06\u00A0iterations provides enough accuracy to calculate up to\u00A02^64 - 1, so the largest power it can compute is\u00A063.\r\n"; }
	else if (word == 'sqrt') {
		buf = "\t\tsqrt (81, 10)\r\n\r\nCalculates an approximate square root of the first argument. This method utilizes the Babylonian Method for computing the square root. The second argument must be an UInt whose value is known at compile time, which represents the number of iterations the algorithm should perform.\r\n\r\nFor reference, when performing 5 iterations, the algorithm can reliably calculate the square root up to 32 squared, or 1,024. When performing 10 iterations, the algorithm can reliably calculate the square root up to 580 squared, or 336,400.\r\n"; }
	else if (word == 'Anybody') {
		buf = "#### Anybody\r\n\r\n    Anybody.publish(); \/\/ race(...Participants).publish()\r\n\r\nReach provides a shorthand,\u00A0`[Anybody](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Anybody%29%29%29)`, which serves as a\u00A0`[race](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28race%29%29%29)`\u00A0between all\u00A0`[Participant](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28.Participant%29%29%29)`s. This shorthand can be useful for situations where it does not matter who\u00A0`[publish](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28publish%29%29%29)`es, such as in a\u00A0`[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)`.";}
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
