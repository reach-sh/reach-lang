// Declare the type here, this way, to avoid an error
// from TypeScript about an invalid index signature.
export const KEYWORD_TO_ITEM_KIND_IMPORT: {
	[keyword: string] : 'Text' | 'Method' |
		'Function' | 'Constructor' | 'Field' |
		'Variable' | 'Class' | 'Interface' |
		'Module' | 'Property' | 'Unit' | 'Value' |
		'Enum' | 'Keyword' | 'Snippet' | 'Color' |
		'File' | 'Reference' | 'Folder' |
		'EnumMember' | 'Constant' | 'Struct' |
		'Event' | 'Operator' | 'TypeParameter'
} = require('../../data/keywordToItemKind.json');

export const KEYWORD_WITH_PERIOD_TO_KEYWORDS_LIST: {
	[key : string]: string[]
} = {};

for (const keyword in KEYWORD_TO_ITEM_KIND_IMPORT) {
	const DOT_DELIMITED_KEYWORDS = keyword.split('.');

	// If a second element exists, this keyword has a
	// '.', so add it to the "smart" keyword list.
	if (DOT_DELIMITED_KEYWORDS[1]) {
		// Include '.' after the keyword, e.g.,
		// 'Reach.', instead of 'Reach', for an
		// easier implementation, later.
		DOT_DELIMITED_KEYWORDS[0] += '.';

		// Does this keyword with a period map to
		// anything, yet? If it doesn't, set it to
		// an empty array. Otherwise, do nothing.
		KEYWORD_WITH_PERIOD_TO_KEYWORDS_LIST[
			DOT_DELIMITED_KEYWORDS[0]
		] ??= [];
		
		KEYWORD_WITH_PERIOD_TO_KEYWORDS_LIST[
			DOT_DELIMITED_KEYWORDS[0]
		].push(DOT_DELIMITED_KEYWORDS[1]);
	}
}

console.debug(KEYWORD_WITH_PERIOD_TO_KEYWORDS_LIST);