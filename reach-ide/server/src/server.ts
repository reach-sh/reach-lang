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

const REACH_TEMP_INDEX_FILE = ".index.rsh.temp";

const { exec } = require("child_process");

const {indexOfRegex, lastIndexOfRegex} = require('index-of-regex')

const fs = require('fs')

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
			textDocumentSync: TextDocumentSyncKind.Full,
			// Tell the client that the server supports code completion
			completionProvider: {
				resolveProvider: true
			},
/*			codeLensProvider : {
				resolveProvider: true
			},
,*/
			hoverProvider : {
				workDoneProgress: false
			},
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

	// Inject association for file type
	exec("mkdir -p .vscode", (error: { message: any; }, stdout: any, stderr: any) => {
		if (error) {
			connection.console.log(`MKDIR VSCODE error: ${error.message}`);
			return;
		}
		if (stderr) {
			connection.console.log(`MKDIR VSCODE  stderr: ${stderr}`);
			return;
		}
		connection.console.log(`MKDIR VSCODE stdout: ${stdout}`);
		appendRshFileAssociation();
	});

	return result;
});

function appendRshFileAssociation(){

	fs.readFile('.vscode/settings.json',function(err: any,content: string){
		var parseJson;
		try {
			parseJson = JSON.parse(content);
		} catch {
			parseJson = {}
		}
		var fileAssoc = parseJson["files.associations"]
		if (fileAssoc == undefined) {
			parseJson["files.associations"] = { "*.rsh" : "javascript" }
		} else {
			parseJson["files.associations"]["*.rsh"] = "javascript";
		}
		fs.writeFile('.vscode/settings.json',JSON.stringify(parseJson),function(err: any){
		  if(err) throw err;
		})
	})

  }

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

	let textDocumentFromURI = documents.get(textDocument.uri)
	let textDocumentContents = textDocumentFromURI?.getText()
	connection.console.log("TEXT DOCUMENT [" + textDocumentContents + "]");

	// In this simple example we get the settings for every validate run.
	let settings = await getDocumentSettings(textDocument.uri);

	let diagnostics: Diagnostic[] = [];

	// Download the Reach shell script if it does not exist
	const path = './reach'
	try {
		if (fs.existsSync(path)) {
			connection.console.log("Reach shell script exists");
		} else {
			connection.console.log("Reach shell script not found, downloading now...");
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
		}
	} catch(err) {
		connection.console.log("Failed to check if Reach shell scripts exists, downloading anyways...");
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
	}
	
	// TODO compile temp file instead of this current file

	connection.console.log(`BEFORE`);

	fs.writeFile(REACH_TEMP_INDEX_FILE, textDocumentContents, function(err: any) {
		if(err) {
			connection.console.log(`Failed to write temp Reach index.rsh file: ${err}`);
			return;
		}
		connection.console.log(`Temp Reach file ${REACH_TEMP_INDEX_FILE} saved!`);
	});

	await exec("./reach compile " + REACH_TEMP_INDEX_FILE, (error: { message: any; }, stdout: any, stderr: any) => {
		if (error) {
			connection.console.log(`FOUND Reach compile error: ${error.message}`);
			let errorLocations : ErrorLocation[] = findErrorLocations(error.message);
			let problems = 0;
			for (var i = 0; i < errorLocations.length; i++) {
				let element : ErrorLocation = errorLocations[i];
				connection.console.log(`FOR EACH LOCATION, ERROR MSG IS ` + element.errorMessage);
				if (problems < settings.maxNumberOfProblems) {
					problems++;
					
					addDiagnostic(element, `${element.errorMessage}`, 'Reach compilation encountered an error.', DiagnosticSeverity.Error, DIAGNOSTIC_TYPE_COMPILE_ERROR + (element.suggestions != undefined ? element.suggestions : "") );
					
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
	suggestions: string | undefined; // e.g. literally this whole thing: "declassify","array","assert","assume","closeTo"
}

function findErrorLocations(compileErrors: string) : ErrorLocation[] {
	connection.console.log(`FIND ERROR LOCATIONS`);
	// GET ERROR MESSAGES BY RUNNING COMPILE


// change the below to run reach compile and get the output e.g. parse from
	/*

	WARNING: Found orphan containers (tut_ethereum-devnet_1) for this project. If you removed or renamed this service in your compose file, you can run this command with the --remove-orphans flag to clean it up.
Creating tut_reach_run ... done
reachc: error: ./.index.rsh.reach-ide.temp:18:23:id ref: Invalid unbound identifier: declaaaaaaaaassify. Did you mean: ["declassify","addressEq","array","assert","assume"]
CallStack (from HasCallStack):
  error, called at src/Reach/AST.hs:58:3 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.AST
  expect_throw, called at src/Reach/Eval.hs:441:7 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.Eval
  env_lookup, called at src/Reach/Eval.hs:1638:41 in reach-0.1.2-KZ4oXxVSV3mFfbu8tz29Bg:Reach.Eval

	*/


	//let text = textDocument.getText();
	let pattern = /error: .*/g  // look for error string
	let m: RegExpExecArray | null;

	let problems = 0;
	let locations: ErrorLocation[] = [];
	while ((m = pattern.exec(compileErrors)) && problems < 100 /*settings.maxNumberOfProblems*/) {
		connection.console.log(`FOUND PATTERN: ${m}`);
	// ERROR MESSAGE m:
    //error: ./index.rsh:13:23:id ref: Invalid unbound identifier: declassiafy. Did you mean: ["declassify","array","assert","assume","closeTo"]

	var start: Position;
	var end: Position;

		// Get actual message portion after the line and position numbers
		var tokens = m[0].split(':');
		connection.console.log(`TOKENS: `+tokens);
		var linePos = parseInt(tokens[2]);
		var charPos = parseInt(tokens[3]);
		var actualMessage = "";
		if (isNaN(linePos) || isNaN(charPos)) { // no line/pos found - treat as generic error
			for (var i=1; i<tokens.length; i++) { // start after "error"
				actualMessage += tokens[i];
				if (i < (tokens.length - 1)) {
					actualMessage += ":"; // add back the colons in between
				}
			}

			start = { line: 0, character: 0 }; // generic error highlights everything
			end = { line: 9999, character: 9999 };
		} else {
			for (var i=4; i<tokens.length; i++) { // start after line/pos
				actualMessage += tokens[i];
				if (i < (tokens.length - 1)) {
					actualMessage += ":"; // add back the colons in between
				}
			}

			start = { line: linePos - 1, character: charPos - 1 } // Reach compiler numbers starts at 1

			// Get list of suggestions from compiler
			const SUGGESTIONS_PREFIX = "Did you mean: [";
			const SUGGESTIONS_SUFFIX = "]";
			var indexOfSuggestions = actualMessage.indexOf(SUGGESTIONS_PREFIX);
			var suggestions;
			if (indexOfSuggestions != -1) {
				suggestions = actualMessage.substring(indexOfSuggestions + SUGGESTIONS_PREFIX.length, actualMessage.lastIndexOf(SUGGESTIONS_SUFFIX));
			}
			connection.console.log(`SUGGESTIONS: ${suggestions}`);

			// Get the problematic string (before the list of suggestions)
			if (suggestions !== undefined) {
				var messageWithoutSuggestions = actualMessage.substring(0, indexOfSuggestions);
				let messageWithoutSuggestionsTokens : string[] = messageWithoutSuggestions.split(" ");
				connection.console.log(`messageWithoutSuggestionsTokens: ${messageWithoutSuggestionsTokens}`);
				var problematicString = messageWithoutSuggestionsTokens[messageWithoutSuggestionsTokens.length - 2]; // last space is a token too
				problematicString = problematicString.substring(0, problematicString.length - 1); // remove trailing period at end of sentence 
				connection.console.log(`PROBLEMATIC STRING: ${problematicString}`);

				end = { line: linePos - 1, character: charPos - 1 + problematicString.length};
			} else {
				end = { line: linePos, character: 0 } // until end of line, or equivalently the next line
			}
		}

		let location: ErrorLocation = {
			range: {
				start: start,
				end: end
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
			let labelPrefix : string = "Replace with ";
			let range : Range = diagnostic.range;
			let possibleReplacements : string = String(diagnostic.code).substring(DIAGNOSTIC_TYPE_COMPILE_ERROR.length);
			if (possibleReplacements.length != 0) {
				// Convert list of suggestions to an array
				// Example input: "declassify","array","assert","assume","closeTo"
				var suggestionsArray = possibleReplacements.split(","); // split by commas
				for (var j=0; j<suggestionsArray.length; j++) {
					suggestionsArray[j] = suggestionsArray[j].substring(1, suggestionsArray[j].length - 1); // remove surrounding quotes

					codeActions.push(getQuickFix(diagnostic, labelPrefix + suggestionsArray[j], range, suggestionsArray[j], textDocument));
				}
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


// This handler provides the initial list of the completion items.
connection.onCompletion(
	async (_textDocumentPosition: TextDocumentPositionParams): Promise<CompletionItem[]> => {
		// The passed parameter contains the position of the text document in
		// which code complete got requested.

		let completionItems : CompletionItem[] = [];

		// Snippets
		{
			let snippet : string = 
				"'reach 0.1';\n" +
				"\n" + 
				"export const main =\n" +
				"  Reach.App(\n" +
				"    {},\n" +
				"    [['Alice', {}], ['Bob', {}]],\n" +
				"    (A, B) => {\n" +
				"	   exit(); });";
			insertSnippet(_textDocumentPosition, snippet, completionItems, undefined, "Reach template", 0);
		}
		
		return completionItems;
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
	async (item: CompletionItem): Promise<CompletionItem> => {
		item.documentation = item.textEdit?.newText;
		return item;
	}
);



connection.onHover(

	async (_params: HoverParams): Promise<Hover> => {
		let textDocument = documents.get(_params.textDocument.uri)
		let position = _params.position
		let hover : Hover = {
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

function isReachKeyword(word: string) : boolean {
	var reachKeywords = [ 'export', 'import', 'Reach.App', 'only', 'each' ];
	for (var i=0; i < reachKeywords.length; i++) {
		if (word == reachKeywords[i]) {
			return true;
		}
	}
	return false;
}

function getReachKeywordMarkdown(word: string) : string {
	// source: https://docs.reach.sh/ref-programs-module.html#%28tech._source._file%29
	// then input to: https://euangoddard.github.io/clipboard2markdown/
	// then input to: https://www.freeformatter.com/javascript-escape.html
	var buf : string = "";
	if (word == 'export') {
		buf = "#### export\r\n\r\nModule-level\u00A0[identifier definitions](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._identifier._definition%29)\u00A0may be\u00A0exported by writing\u00A0`export`\u00A0in front of them. For example,\r\n\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 1;\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) [a, b, ...more] = [ 0, 1, 2, 3, 4 ];\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [function](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28function%29%29%29) add1(x) { [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) x [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 1; };\r\n\r\nare valid\u00A0[exports](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29).\r\n\r\nModule-level identifiers may also be\u00A0[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29)ed after the fact, and may be renamed during export. For example:\r\n\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) w = 2;\r\n[const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) z = 0;\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) {w, z as zero};\r\n\r\nIdentifiers from other modules may be re-exported (and renamed), even if they are not imported in the current module. For example:\r\n\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) {u, x as other_x} [from](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28from%29%29%29) \'.\/other-module.rsh\';\r\n\r\nAn\u00A0[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29)ed identifier in a given\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29)\u00A0may be\u00A0[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._import%29)ed by other\u00A0[modules](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29).";
	} else if (word == 'import') {
		buf = "#### import\r\n\r\nimport \'games-of-chance.rsh\';\r\n\r\nWhen a\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29),\u00A0X, contains an\u00A0import, written\u00A0`[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28import%29%29%29) \"LIB.rsh\";`, then the path\u00A0\"LIB.rsh\"\u00A0must resolve to another Reach\u00A0[source file](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._source._file%29). The\u00A0[exports](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29)\u00A0from the\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29)\u00A0defined by\u00A0\"LIB.rsh\"\u00A0are included in the set of\u00A0[bound identifier](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._bound._identifier%29)s in\u00A0X.\r\n\r\n[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28import%29%29%29) {flipCoin, rollDice as d6} from \'games-of-chance.rsh\';\r\n\r\nImport statements may limit or rename the imported\u00A0[identifiers](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._identifier%29).\r\n\r\n[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28import%29%29%29) [*](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2A%29%29%29) as gamesOfChance [from](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28from%29%29%29) \'games-of-chance.rsh\';\r\n\r\nImports may instead bind the entire\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29)\u00A0to a single\u00A0[identifier](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._identifier%29), which is an\u00A0[object](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._object%29)\u00A0with\u00A0[fields](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._field%29)\u00A0corresponding to that\u00A0[module](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._module%29)\'s\u00A0[exports](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29).\r\n\r\n[Import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._import%29)\u00A0cycles are\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29).\r\n\r\nThe path given to an\u00A0[import](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._import%29)\u00A0may\u00A0not\u00A0include\u00A0..\u00A0to specify files outside the current directory\u00A0nor\u00A0may it be an absolute path.\r\n\r\nIt\u00A0must\u00A0be a relative path, which is resolved relative to the parent directory of the\u00A0[source file](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._source._file%29)\u00A0in which they appear.";
	} else if (word == 'Reach.App') {
		buf = "##### Reach.App\r\n\r\n[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28export%29%29%29) [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) main =\r\n  Reach.App({}, [[\"A\", {displayResult: [Fun](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Fun%29%29%29)(Int, [Null](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.Null%29%29%29))}]], (A) [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n    [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) result = 0;\r\n    A.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) { [interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).displayResult(result); })\r\n    [return](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28return%29%29%29) result;\r\n  });\r\n\r\nReach.App\u00A0is a function which accepts three arguments:\u00A0`options`,\u00A0`participantDefinitions`, and\u00A0`program`.\r\n\r\nThe\u00A0`options`\u00A0must be an object. It supports the following options:\r\n\r\n|\r\n\r\n`deployMode`\r\n\r\n |  |\r\n\r\n`\'constructor\'`\u00A0(default) or\u00A0`\'firstMsg\'`\r\n\r\n |  |\r\n\r\nDetermines whether\u00A0[contract](https:\/\/docs.reach.sh\/ref-model.html#%28tech._contract%29)\u00A0should be\u00A0[deploy](https:\/\/docs.reach.sh\/ref-model.html#%28tech._deploy%29)ed independently (`\'constructor\'`) or as part of the first\u00A0[publication](https:\/\/docs.reach.sh\/ref-model.html#%28tech._publication%29)\u00A0(`\'firstMsg\'`). If deployed as part of the first publication, then the first publication must precede all uses of\u00A0`[wait](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28wait%29%29%29)`\u00A0and\u00A0`.[timeout](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28timeout%29%29%29)`. See\u00A0[the guide on deployment modes](https:\/\/docs.reach.sh\/guide-deploymode.html)\u00A0for a discussion of why to choose a particular mode.\r\n\r\n |\r\n|  |  |  |  |  |\r\n|\r\n\r\n`verifyOverflow`\r\n\r\n |  |\r\n\r\n`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0or\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`\u00A0(default)\r\n\r\n |  |\r\n\r\nDetermines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29).max`. This defaults to\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`, because it is onerous to verify. We recommend turning it on before final deployment, but leaving it off during development. When it is\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`,\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0will ensure that overflows do not actually occur on the network.\r\n\r\n |\r\n|  |  |  |  |  |\r\n|\r\n\r\n`verifyPerConnector`\r\n\r\n |  |\r\n\r\n`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`\u00A0or\u00A0`[false](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28false%29%29%29)`\u00A0(default)\r\n\r\n |  |\r\n\r\nDetermines whether verification is done per connector, or once for a generic connector. When this is\u00A0`[true](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28true%29%29%29)`, then connector-specific constants, like\u00A0`[UInt](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28.U.Int%29%29%29).max`, will be instatiated to literal numbers. This concretization of these constants can induce performance degradation in the verifier.\r\n\r\n |\r\n|  |  |  |  |  |\r\n|\r\n\r\n`connectors`\r\n\r\n |  |\r\n\r\n`[ETH, ALGO]`\u00A0(default)\r\n\r\n |  |\r\n\r\nA tuple of the\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0that the application should be compiled for. By default, all available\u00A0[connectors](https:\/\/docs.reach.sh\/ref-model.html#%28tech._connector%29)\u00A0are chosen.\r\n\r\n |\r\n\r\nThe\u00A0`participantDefinitions`\u00A0argument is an tuple of tuples. Each tuple is a pair of\u00A0`participantName`\u00A0and\u00A0`participantInteractInterface`.\u00A0`participantName`\u00A0is a string which indicates the name of the participant function in the generated\u00A0[backend](https:\/\/docs.reach.sh\/ref-model.html#%28tech._backend%29)\u00A0code. Each\u00A0`participantName`\u00A0must be unique.\u00A0`participantInteractInterface`\u00A0is a\u00A0participant interact interface, an object where each field indicates the type of a function or value which must be provided to the\u00A0[backend](https:\/\/docs.reach.sh\/ref-model.html#%28tech._backend%29)\u00A0by the\u00A0[frontend](https:\/\/docs.reach.sh\/ref-model.html#%28tech._frontend%29)\u00A0for\u00A0[interact](https:\/\/docs.reach.sh\/ref-model.html#%28tech._interact%29)ing with the participant.\r\n\r\nThe\u00A0`program`\u00A0argument must be a syntactic\u00A0[arrow expression](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._arrow._expression%29). The arguments to this arrow must match the number and order of\u00A0`participantDefinitions`. The function body is the program to be\u00A0[compile](https:\/\/docs.reach.sh\/ref-model.html#%28tech._compile%29)d. It specifies a\u00A0[step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._step%29), which means its content is specified by\u00A0[Steps](https:\/\/docs.reach.sh\/ref-programs-step.html).\r\n\r\nIf the result of\u00A0`[Reach](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28.Reach%29%29%29).[App](https:\/\/docs.reach.sh\/ref-programs-module.html#%28reach._%28%28.App%29%29%29)`\u00A0is eventually bound to an identifier that is\u00A0[export](https:\/\/docs.reach.sh\/ref-programs-module.html#%28tech._export%29)ed, then it may be a target given to the compiler, as discussed in\u00A0[the section on usage](https:\/\/docs.reach.sh\/ref-usage.html#%28part._ref-usage-compile%29).";
	} else if (word == 'only' || word == 'each') {
		buf = "##### only\u00A0and\u00A0each\r\n\r\nAlice.only(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) pretzel = [interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).random(); });\r\n\r\nA\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29)\u00A0statement is written\u00A0`PART.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) BLOCK)`, where\u00A0`PART`\u00A0is a\u00A0[participant](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0identifier and\u00A0`BLOCK`\u00A0is a\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29). Any bindings defined within the\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29)\u00A0of a\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29)\u00A0are available in the\u00A0[statement](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._statement%29)\'s\u00A0[tail](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._tail%29)\u00A0as new\u00A0[local state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._state%29). For example,\r\n\r\nAlice.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 3; });\r\nAlice.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) y = x [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 1; });\r\n\r\nis a\u00A0[valid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._valid%29)\u00A0program where\u00A0`Alice`\'s\u00A0[local state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._state%29)\u00A0includes the\u00A0[private](https:\/\/docs.reach.sh\/ref-model.html#%28tech._private%29)\u00A0values\u00A0`x`\u00A0(bound to\u00A0`3`) and\u00A0`y`\u00A0(bound to\u00A0`4`). However, such bindings are\u00A0not\u00A0[consensus state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._consensus._state%29), so they are purely\u00A0[local state](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._state%29). For example,\r\n\r\nAlice.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) x = 3; });\r\nBob.[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)(() [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) y = x [+](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28%2B%29%29%29) 1; });\r\n\r\nis an\u00A0[invalid](https:\/\/docs.reach.sh\/ref-programs-valid.html#%28tech._invalid%29)\u00A0program, because\u00A0`Bob`\u00A0does not know\u00A0`x`.\r\n\r\n---\r\n\r\neach([Alice, Bob], () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) {\r\n  [const](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28const%29%29%29) pretzel = [interact](https:\/\/docs.reach.sh\/ref-programs-local.html#%28reach._%28%28interact%29%29%29).random(); });\r\n\r\nAn\u00A0each\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29)\u00A0statement can be written as\u00A0`[each](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28each%29%29%29)(PART_TUPLE () [=>](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28reach._%28%28~3d._~3e%29%29%29) BLOCK)`, where\u00A0`PART_TUPLE`\u00A0is a tuple of\u00A0[participants](https:\/\/docs.reach.sh\/ref-model.html#%28tech._participant%29)\u00A0and\u00A0`BLOCK`\u00A0is a\u00A0[block](https:\/\/docs.reach.sh\/ref-programs-compute.html#%28tech._block%29). It is an abbreviation of many\u00A0[local step](https:\/\/docs.reach.sh\/ref-model.html#%28tech._local._step%29)\u00A0statements that could have been written with\u00A0`[only](https:\/\/docs.reach.sh\/ref-programs-step.html#%28reach._%28%28only%29%29%29)`."
	}
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
