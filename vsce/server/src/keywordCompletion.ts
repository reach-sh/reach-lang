import { CompletionItemKind } from 'vscode-languageserver';

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

export const KEYWORD_TO_COMPLETION_ITEM_KIND: CompletionMap = {};

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
