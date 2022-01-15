// https://github.com/microsoft/vscode-eslint/blob/main/server/webpack.config.js
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

//@ts-check

'use strict';

const { join } = require('path');
const SHARED_WEBPACK_CONFIG_FUNCTION = require(
	'../shared.webpack.config'
);

module.exports = SHARED_WEBPACK_CONFIG_FUNCTION({
	context: join(__dirname),
	entry: {
		extension: './src/server.ts',
	},
	output: {
		filename: 'server.js',
		path: join(__dirname, 'out')
	}
});