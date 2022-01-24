const path = require('path');
var webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  entry: './src/index.mjs',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
  },
  mode: 'none',
  devtool: "source-map",
  experiments: {
    topLevelAwait: true
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.platform': JSON.stringify('linux')
    }),
    new CopyWebpackPlugin({
      patterns: [
       { from: './static/favicon.ico' },
      ]
    })
  ],
  module: {
    rules: [
      { test: /\.css$/, use: 'css-loader' },
      {
        test: /\.(scss)$/,
        use: [{
          // inject CSS to page
          loader: 'style-loader'
        }, {
          // translates CSS into CommonJS modules
          loader: 'css-loader'
        }, {
          // Run postcss actions
          loader: 'postcss-loader',
          options: {
            postcssOptions: {
              plugins: function () {
                return [
                  require('autoprefixer')
                ];
              }
            }
          }
        }, {
          // compiles Sass to CSS
          loader: 'sass-loader'
        }]
      }
    ]
  }
};
