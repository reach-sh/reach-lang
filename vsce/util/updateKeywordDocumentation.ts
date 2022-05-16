import { readFileSync, writeFileSync } from 'fs';
import { basename } from 'path';

type PrintKeywordInfoJSON = {
  [key: string] : {
    'CompletionItemKind': string,
  }
};

type XrefsJSON = {
  [key: string] : {
    'title': string,
    'path': string
  }
};

const MOST_KEYWORDS: PrintKeywordInfoJSON = require(
  '../data/print-keyword-info.json'
);
const XREFS_JSON: XrefsJSON = require(
  '../../docs/xrefs.json'
);

// @TODO: Maybe after CORE-1565, manually adding
// these few keywords won't be necessary?
const ALL_KEYWORDS = [
  ...Object.keys(MOST_KEYWORDS),
  'each',
  'interact',
  'only',
  'pay',
  'publish',
  'timeout',
  'when',
];

const DOCUMENTATION_DIRECTORY_ROOT = '../docs/src/rsh';

const DOCUMENTATION_DIRECTORIES: string[] = [
  `${DOCUMENTATION_DIRECTORY_ROOT}/appinit`,
  `${DOCUMENTATION_DIRECTORY_ROOT}/compute`,
  `${DOCUMENTATION_DIRECTORY_ROOT}/consensus`,
  `${DOCUMENTATION_DIRECTORY_ROOT}/errors`,
  `${DOCUMENTATION_DIRECTORY_ROOT}/local`,
  `${DOCUMENTATION_DIRECTORY_ROOT}/module`,
  `${DOCUMENTATION_DIRECTORY_ROOT}/step`,
];

const GET_DOCUMENTATION_FOR = (
  keyword: string,
  docDirs: string[],
  xrefsJson: XrefsJSON,
  docsSiteBaseUrl: string
) => {
  const searchString = `@{ref("rsh", "${keyword}")}`;

  for (const documentationDirectory of docDirs) {
    const fileToSearch = `${documentationDirectory}/index.md`;

    const stringToSearch = readFileSync(
      fileToSearch
    ).toString();

    const i = stringToSearch.indexOf(searchString);

    const weFoundAMatch = i !== -1;

    if (weFoundAMatch) {
      const docStartingAtKeyword = stringToSearch.slice(i);
      const endIndex = docStartingAtKeyword.indexOf('\n#');

      const thisKeyWordsDocs = docStartingAtKeyword.slice(
        0, endIndex
      );

      return PROCESS(
        thisKeyWordsDocs, xrefsJson, docsSiteBaseUrl
      );
    }
  }

  let errorString = "Whoa! Didn't find documentation for keyword\n";
  errorString += keyword + '\n';
  errorString += `despite searching for ${
    searchString
  } in these ${
    DOCUMENTATION_DIRECTORIES.length
  } directories:\n`;
  DOCUMENTATION_DIRECTORIES.forEach(docDir => {
    errorString += docDir + '\n'
  });

  throw new Error(errorString);
};

const PROCESS = (
  stringToProcess: string,
  xrefsJson: XrefsJSON,
  docsSiteBaseUrl: string
): string => {
  // VSCode hover documentation doesn't know about Reach
  // syntax, so use JavaScript syntax highlighting.
  const jsInsteadOfReach = stringToProcess.replace(
    /``` *reach/g, '```javascript'
  );

  /**
   * For example, replace `@{defn("remote functions")}`
   * with `remote functions`, and replace
   * `@{defn("Reach.App")}` with `Reach.App`.
   */
  const withoutAtDefn = jsInsteadOfReach.replace(
    /@{defn\("([a-zA-Z \.]+)"\)}/g, '$1'
  );

  // These don't do anything for VSCode hover documentation!
  const withoutRsh = withoutAtDefn.replace(/{!rsh} /g, '');
  const withoutJs = withoutRsh.replace(/{!js} /g, '');

  /**
   * Delete things like `@{ref("rsh", "UInt256.max")}`,
   * for example.
   */
  const withoutAtRef = withoutJs.replace(
    /@{ref\("rsh", "[a-zA-Z0-9\.]+"\)}/g, ''
  );

  // Make :::note ... ::: look nice in hover documentation!
  // https://stackoverflow.com/a/41449789
  const modifyNotePart1Of2 = withoutAtRef.replace(
    /:::note/g, '---\n**NOTE**\n'
  );
  const modifyNotePart2Of2 = modifyNotePart1Of2.replace(
    /:::/g, '\n---'
  );

  // Tooltips look like links, but clicking them
  // doesn't lead users anywhere useful.
  // So, replace
  // `@{tooltip("exports", "exporting allows ...")}`
  // with `exports`, for example.
  const tooltipRemoved = modifyNotePart2Of2.replace(
    /@{tooltip\("([a-z]+)", "[a-z ]+"\)}/g, '$1'
  );

  const handleDoubleHashLink = HANDLE_DOUBLE_HASH_LINKS_IN(
    tooltipRemoved, xrefsJson, docsSiteBaseUrl
  );

  const handleAtSeclinks = HANDLE_SECLINKS_IN(
    handleDoubleHashLink, xrefsJson, docsSiteBaseUrl
  );

  const sampleProgramInjected = INJECT_SAMPLE_PROGRAMS_INTO(
    handleAtSeclinks
  );

  return sampleProgramInjected;
};

const HANDLE_DOUBLE_HASH_LINKS_IN = (
  docString: string,
  xrefsJson: XrefsJSON,
  docsSiteBaseUrl: string
): string => {
  const regularExpression = RegExp(/]\(##(.+)\)/, 'g');

  let regExArray = regularExpression.exec(docString);
  while (regExArray) {
    const [ _fullMatch, xrefsKey ] = regExArray;
    const { path } = xrefsJson[xrefsKey];
    const docsSiteLink = `${docsSiteBaseUrl}${path}`;
    docString = docString.replace(`##${
      xrefsKey
    }`, docsSiteLink);
    regExArray = regularExpression.exec(docString);
  }

  return docString;
};

const HANDLE_SECLINKS_IN = (
  docString: string,
  xrefsJson: XrefsJSON,
  docsSiteBaseUrl: string
): string => {
  const regularExpression: RegExp = RegExp(
    /@{seclink\(\"([a-z -]+)\"\)}/, 'g'
  );

  let regExArray = regularExpression.exec(docString);
  while (regExArray) {
    const [ _fullMatch, xrefsKey ] = regExArray;
    const { title, path } = xrefsJson[xrefsKey];
    const docsSiteLink = `${docsSiteBaseUrl}${path}`;
    docString = docString.replace(`@{seclink\(\"${
      xrefsKey
    }\"\)}`, `[${title}](${docsSiteLink})`);
    regExArray = regularExpression.exec(docString);
  }

  return docString;
};

const INJECT_SAMPLE_PROGRAMS_INTO = (
  docString: string
): string => {
  const regularExpression: RegExp = RegExp(
    /load: (.+)\n *range: (\d+) *- *(\d+)/, 'g'
  );

  let regExArray = regularExpression.exec(docString);
  while (regExArray) {
    const [
      _fullMatch, programPath, startLine, endLine
    ] = regExArray;
    const pathToRead: string = `../${programPath}`;
    const programToReadLineByLine: string[] = readFileSync(
      pathToRead
    ).toString().split('\n');
    const stringToInject: string = programToReadLineByLine.slice(
      parseInt(startLine) - 1, parseInt(endLine)
    ).join('\n');
    docString = docString.replace(
      /load: (.+)\n *range: (\d+) *- *(\d+)/, stringToInject
    );

    regExArray = regularExpression.exec(docString);
  }

  return docString;
};

const DOCS_SITE_BASE_URL: string = 'https://docs.reach.sh';
const KEYWORD_TO_DOCUMENTATION: {
  [key: string]: string
} = {};

let numberOfKeywordsSkipped = 0;

ALL_KEYWORDS.forEach(keyword => {
  if (keyword.includes('_')) {
    console.info(`Skipping ${keyword}`);
    numberOfKeywordsSkipped++;
  } else {
    const documentation = GET_DOCUMENTATION_FOR(
      keyword,
      DOCUMENTATION_DIRECTORIES,
      XREFS_JSON,
      DOCS_SITE_BASE_URL
    );
    KEYWORD_TO_DOCUMENTATION[keyword] = documentation;
  }
});

console.info(`\nWe skipped ${
  numberOfKeywordsSkipped
} keywords, out of ${
  ALL_KEYWORDS.length
} total.`);

const THIS_PROGRAMS_NAME = basename(__filename);
const PATH_TO_WRITE_TO =
  './server/src/keywordToDocumentation.ts';

let typeScriptSourceString = '/*\n';
typeScriptSourceString += 'A program called\n';
typeScriptSourceString += THIS_PROGRAMS_NAME + '\n';
typeScriptSourceString += 'wrote this file.\n';
typeScriptSourceString += '\n';
typeScriptSourceString += 'Do not modify this file directly.\n';
typeScriptSourceString += '*/\n';
typeScriptSourceString += '\n';
typeScriptSourceString += 'const KEYWORD_TO_DOCUMENTATION: {\n';
typeScriptSourceString += '  [key: string] : string\n';
typeScriptSourceString += '} = ';
typeScriptSourceString += JSON.stringify(
  KEYWORD_TO_DOCUMENTATION, null, 2
);
typeScriptSourceString += ';\n';
typeScriptSourceString += '\n';
typeScriptSourceString +=
  'export default KEYWORD_TO_DOCUMENTATION;\n';

writeFileSync(PATH_TO_WRITE_TO, typeScriptSourceString);

console.info('\nAll done!');
