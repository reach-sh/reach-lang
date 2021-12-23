const path = require('path');
var webpack = require('webpack');

module.exports = {
  entry: './src/index.js',
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
};
