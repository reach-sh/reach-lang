import { CompletionItemKind } from 'vscode-languageserver';

// Use a `require` statement instead of an `import`
// statement to avoid the following error from TypeScript:
// File '../../data/arrayMethods.json' is not under 'rootDir'
// 'rootDir' is expected to contain all source files.ts(6059)
const ARRAY_METHODS: string[] = require(
  "../../data/arrayMethods.json"
);
// Declare the type here, this way, to avoid an error
// from TypeScript later about an invalid index signature.
const KEYWORD_TO_ITEM_KIND_IMPORT: {
  [keyword: string] : "Text" | "Method" | "Function" |
    "Constructor" | "Field" | "Variable" | "Class" |
    "Interface" | "Module" | "Property" | "Unit" | "Value" |
    "Enum" | "Keyword" | "Snippet" | "Color" | "File" |
    "Reference" | "Folder" | "EnumMember" | "Constant" |
    "Struct" | "Event" | "Operator" | "TypeParameter"
} = require(
  "../../data/keywordToItemKind.json"
);
type CompletionMap = { [key : string]: CompletionItemKind }

const ARRAY_METHOD_TO_COMPLETION_ITEM_KIND: CompletionMap = {};

ARRAY_METHODS.forEach(methodName =>
  ARRAY_METHOD_TO_COMPLETION_ITEM_KIND[methodName]
    = CompletionItemKind.Method
);

export const addPrefix = (o: CompletionMap, s: string) =>
  Object.entries(o).reduce((acc, [k, v]) => {
    acc[s + "." + k] = v;
    return acc;
  }, {} as CompletionMap);

export const KEYWORD_TO_COMPLETION_ITEM_KIND: CompletionMap = {
  ...ARRAY_METHOD_TO_COMPLETION_ITEM_KIND,
  ...addPrefix(ARRAY_METHOD_TO_COMPLETION_ITEM_KIND, 'Array'),
};

for (
  const [keyword, itemKind] of Object.entries(
    KEYWORD_TO_ITEM_KIND_IMPORT
  )
) {
  KEYWORD_TO_COMPLETION_ITEM_KIND[keyword]
    = CompletionItemKind[itemKind];
}

export const REACH_KEYWORDS = Object.keys(
  KEYWORD_TO_COMPLETION_ITEM_KIND
);
