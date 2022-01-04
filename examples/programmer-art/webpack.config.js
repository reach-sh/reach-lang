const path = require('path');
var webpack = require('webpack');

module.exports = {
  entry: './src/index.mjs',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
  },
  mode: 'none',
  devtool: "source-map",
  plugins: [
    new webpack.DefinePlugin({
      'process.platform': JSON.stringify('linux')
    })
  ],
  module: {
    rules: [
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
