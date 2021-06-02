const path = require('path');
const webpack = require('webpack');

module.exports = {
  mode: 'production',
  entry: './index.ts',
  output: {
    filename: 'reachsdk.min.js',
    path: path.resolve(__dirname, 'dist/browser'),
    library: {
      type: 'umd',
      name: 'reachsdk',
    },
  },
  devtool: 'source-map',
  resolve: {
    // Add '.ts' as resolvable extensions
    extensions: ['.ts', '.js'],

    fallback: {
      crypto: require.resolve('crypto-browserify'),
      http: require.resolve('stream-http'),
      // net: false,
      stream: require.resolve('stream-browserify'),
      buffer: require.resolve('buffer/'),
      util: false, // require.resolve('util/') ?
      url: false, // require.resolve('url/') ?
      process: require.resolve('process/browser'),
      // path: require.resolve('path-browserify'),
    },
    alias: {
      'wait-port': false,
    }
  },
  // plugins: [
  //   new webpack.ProvidePlugin({
  //     Buffer: ['buffer', 'Buffer'],
  //   }),
  // ],
  module: {
    rules: [
      // All files with a '.ts' extension will be handled by 'ts-loader'.
      {
        test: /\.ts$/,
        loader: 'ts-loader',
        options: {
          configFile: path.resolve(__dirname, 'tsconfig-browser.json'),
        },
      },

      // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
      { test: /\.js$/, loader: 'source-map-loader' },
    ],
  },
};
