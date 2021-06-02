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
    // falling back to browser shims for these
    fallback: {
      crypto: require.resolve('crypto-browserify'),
      http: require.resolve('stream-http'),
      stream: require.resolve('stream-browserify'),
      process: require.resolve('process/browser'),
    },
    // explicitly disabling these
    alias: {
      'wait-port': false,
      util: false,
      url: false,
      net: false,
    }
  },
  // More browser shim stuff
  plugins: [
    new webpack.ProvidePlugin({
      Buffer: ['buffer', 'Buffer'],
    }),
  ],
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
