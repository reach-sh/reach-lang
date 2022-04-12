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

import {
  exec,
  ExecException,
  spawnSync
} from "child_process";
import { tmpdir } from 'os';
import { join, resolve } from 'path';

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
import { For } from './constants';

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
const INDEX_RSH = "index.rsh";


const { indexOfRegex, lastIndexOfRegex } = require('index-of-regex')

const fs = require('fs')

let reachTempIndexFile: string;
let tempFolder: string;
let workspaceFolder: string;

connection.onInitialize((params: InitializeParams) => {

  fs.mkdtemp(join(tmpdir(), 'reach-ide-'), (err: string, folder: string) => {
    if (err) {
      connection.console.error(err);
      throw err;
    }
    // Use a format like 'reach-ide-SUFFIX/reach-ide'
    tempFolder = join(folder, 'reach-ide');
    fs.mkdir(tempFolder, (err2:string) => {
      if (err2) {
        connection.console.error(err2);
        throw err2;
      }
      connection.console.log("Temp folder: " + tempFolder);
      reachTempIndexFile = join(tempFolder, INDEX_RSH);
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
      // codeLensProvider : {
      //   resolveProvider: true
      // },
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
  relativePathToShellScript: string;
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
  defaultSettings = {
    maxNumberOfProblems: DEFAULT_MAX_PROBLEMS,
    relativePathToShellScript: './reach',
  };

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

const DOCKER_IS_NOT_RUNNING = (): boolean => {
  const dockerIsRunning: boolean = spawnSync(
    'docker', [ '--version' ]
  ).status === 0;

  return dockerIsRunning === false;
};

let theCompilerIsCompiling  = false;
let weNeedToCompileAgain    = false;

async function validateTextDocument(textDocument: TextDocument): Promise<void> {

  let textDocumentFromURI = documents.get(textDocument.uri)
  let textDocumentContents = textDocumentFromURI?.getText()

  // In this simple example we get the settings for every validate run.
  let settings = await getDocumentSettings(textDocument.uri);

  let diagnostics: Diagnostic[] = [];

  const { relativePathToShellScript } = settings;

  const absolutePathToShellScript = resolve(
    relativePathToShellScript
  );

  // Download the Reach shell script if it does not exist
  try {
    if (fs.existsSync(absolutePathToShellScript)) {
      connection.console.info(
        'Reach shell script exists at'
      );
      connection.console.info(absolutePathToShellScript);
    } else {
      connection.console.log('');
      connection.console.error(
        'Failed to find reach shell script at'
      );
      connection.console.error(absolutePathToShellScript);
      connection.console.error(
        'Attempting to download Reach shell script to'
      );
      connection.console.error(absolutePathToShellScript);
      connection.console.error('now...\n');
      exec(
        'curl https://raw.githubusercontent.com/' +
        'reach-sh/reach-lang/master/reach -o ' +
        absolutePathToShellScript + ' ; chmod +x ' +
        absolutePathToShellScript, (
          error: ExecException | null,
          stdout: string,
          stderr: string
        ) => {
          if (error) {
            connection.console.error(
              `Reach download error in try: ${
                error.message
              }`
            );
            connection.sendNotification(
              For.compilationCouldntCommence, error.message
            );
            return;
          }

          if (stderr) {
            connection.console.error(
              `Reach download stderr in try: ${
                stderr
              }`
            );
            connection.sendNotification(
              For.compilationCouldntCommence, stderr
            );
            return;
          }

          connection.console.log(
            `Reach download stdout in try: ${
              stdout
            }`
          );
          connection.sendNotification(
            For.compilationCouldntCommence, stdout
          );
        }
      );
    }
  } catch (err) {
    connection.console.log('');
    connection.console.error(
      'Failed to check if reach shell script exists at'
    );
    connection.console.error(absolutePathToShellScript);
    connection.console.error(`due to error: ${err}`);
    connection.console.error(
      'Attempting to download reach shell script to'
    );
    connection.console.error(absolutePathToShellScript);
    connection.console.error('now...\n');
    exec(
      'curl https://raw.githubusercontent.com/' +
      'reach-sh/reach-lang/master/reach -o ' +
      absolutePathToShellScript + ' ; chmod +x ' +
      absolutePathToShellScript, (
        error: ExecException | null,
        stdout: string,
        stderr: string
      ) => {
        if (error) {
          connection.console.error(
            `Reach download error in catch: ${
              error.message
            }`
          );
          return;
        }
        if (stderr) {
          connection.console.error(
            `Reach download stderr in catch: ${
              stderr
            }`
          );
          return;
        }
        connection.console.log(
          `Reach download stdout in catch: ${
            stdout
          }`
        );
      }
    );
  }

  // Compile temp file instead of this current file

  fs.writeFile(reachTempIndexFile, textDocumentContents, function (err: any) {
    if (err) {
      connection.console.error(`Failed to write temp source file: ${err}`);
      return;
    }
    connection.console.log(`Temp source file ${reachTempIndexFile} saved!`);
  });

  if (theCompilerIsCompiling) {
    weNeedToCompileAgain = true;
    console.debug(
      "Compilation already in process; will recompile",
      new Date().toLocaleTimeString()
    );
    return;
  }

  theCompilerIsCompiling = true;

  // Add blank lines for readability.
  connection.console.log('');
  connection.console.info(`Compiling ${INDEX_RSH} in`);
  connection.console.info(tempFolder);
  connection.console.info('using');
  connection.console.info(absolutePathToShellScript);
  connection.console.log('');

  connection.sendNotification(For.commencedCompilation);
  exec(
    "cd " + tempFolder + " && " + absolutePathToShellScript
    + " compile " + INDEX_RSH +
    " --error-format-json --stop-after-eval", (
      error: ExecException | null,
      stdout: string,
      stderr: string
    ) => {
      // This callback function should execute exactly
      // when this compilation command has finished.
      // "child_process.exec(): spawns a shell...
      // passing the stdout and stderr to a callback
      // function when complete". See
      // https://nodejs.org/api/child_process.html
      // Add blank lines for readability.
      connection.console.log('');
      connection.console.info('Compilation completed!');
      connection.console.log('');

      theCompilerIsCompiling = false;
      if (weNeedToCompileAgain) {
        weNeedToCompileAgain = false;
        validateTextDocument(textDocument);
        return;
      }

      if (error) {
        connection.console.error(`Found compile error: ${
          error.message
        }`);

        if (DOCKER_IS_NOT_RUNNING()) {
          const s =  "Docker doesn't seem to be running.";
          connection.sendNotification(
            For.compilationCouldntCommence, s
          );
          connection.console.error(s);
          return;
        }

        const errorLocations: ErrorLocation[]
          = findErrorLocations(error.message);
        let problems = 0;
        errorLocations.forEach(err => {
          connection.sendNotification(
            For.errorDuringCompilation, err.errorMessage
          );
          connection.console.log(
            `Displaying error message: ` +
            err.errorMessage
          );
          let maxProblems =
            settings?.maxNumberOfProblems ||
            DEFAULT_MAX_PROBLEMS;
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
      connection.console.log(`Reach compiler output: ${
        stdout
      }`);
      connection.sendNotification(For.completedCompilation);
    }
  );

  // Send the computed diagnostics to VSCode (before the above promise finishes, just to clear stuff).
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });

  function addDiagnostic(element: ErrorLocation, message: string, details: string, severity: DiagnosticSeverity, code: string | undefined, suggestions: string[], source: string) {
    const href = `https://docs.reach.sh/rsh/errors/#${code}`;
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
  connection.console.log('We received a file change event.');
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
      : { line: linePos + 1, character: 0 };

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
    } else if (diagnostic.code === 'RE0002') {
      // Grab the general starting area of
      // the diagnostic.
      const { start } = range;

      // Then, start analyzing text from
      // the *beginning* of the line
      // containing the Diagnostic, since
      // the Diagnostic may start in the
      // middle of a declaration, i.e., it
      // may not contain "let", especially
      // if indented, like "    let ...".
      start.character = 0;

      const text = textDocument.getText(range);

      // This edit does not handle all possible
      // causes of RE0002! It only handles what
      // I imagine could be a common error where
      // developers assume "let" is a keyword.
      const newText = text.replace('let', 'var');

      if (newText !== text) {
        const textEdit: TextEdit = {
          newText, range,
        };
        const changes = {
          [ textDocument.uri ]: [ textEdit ]
        };
        const edit: WorkspaceEdit = { changes };
        const title = "Change 'let' to 'var'.";
        const codeAction: CodeAction = {
          diagnostics: [ diagnostic ],
          edit,
          kind: CodeActionKind.QuickFix,
          title
        };
        codeActions.push(codeAction);
      }
    } else if (diagnostic.code === "RE0048") {
      const title = "Add Reach program header.";

      // Extract the error message from Reach's
      // compiler, from the diagnostic variable.
      const { message } = diagnostic;

      // This is a relatively robust regular
      // expression. It'll find a match even if
      // the compiler's error message changes to
      // "reach 0.1";, 'reach 0.1', `reach 0.1`,
      // or "reach 1001.74", for example.
      const regEx: RegExp =
        /('|"|`)reach \d+\.\d+\1;?/;
      const regExMatch = message.match(regEx);

      // If, for some reason, a match doesn't
      // exist, which should never happen, use
      // 'reach 0.1'; as fallback text.
      let newText: string = "'reach 0.1';";
      if (regExMatch) {
        newText = regExMatch[0];
        console.debug(newText, regExMatch);
      }
      // Add newlines to improve readability.
      newText += "\n\n";

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
