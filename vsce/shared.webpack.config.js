// https://github.com/microsoft/vscode-eslint/blob/main/shared.webpack.config.js
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

//@ts-check
/** @typedef {import('webpack').Configuration} WebpackConfig **/

'use strict';

const { join } = require('path');
const MERGE_MODULE = require('merge-options');

module.exports = (/**@type WebpackConfig*/extConfig) => {
	/** @type WebpackConfig */
	let defaultConfig = {
		// This leaves the source code as
		// close as possible to the original.
		// When packaging, we set this to 'production'.
		mode: 'none',
		// Extensions run in a Node context.
		target: 'node',
		node: {
			// Leave the __dirname-behaviour intact.
			__dirname: false
		},
		resolve: {
			mainFields: ['module', 'main'],
			// Support .ts files and .js files.
			extensions: ['.ts', '.js']
		},
		module: {
			rules: [{
				test: /\.ts$/,
				exclude: /node_modules/,
				use: [{
					// configure TypeScript loader:
					// * enable sources maps for
					// end-to-end source maps
					loader: 'ts-loader',
					options: {
						compilerOptions: {
							"sourceMap": true,
						}
					}
				}]
			}]
		},
		externals: {
			// ignored because it doesn't exist
			'vscode': 'commonjs vscode',
		},
		output: {
			// All output goes into `dist`.
			// Packaging depends on that,
			// and this must always be like it.
			filename: '[name].js',
			path: join(extConfig.context, 'out'),
			libraryTarget: "commonjs",
		},
		// yes, really source maps
		devtool: 'source-map'
	};

	return MERGE_MODULE(defaultConfig, extConfig);
};