webpack = require('webpack');
module.exports = {
  entry: './main.js',
  output: {
  	filename: 'bundle.js'
  },
  resolve: {
    fallback: {
      process: require.resolve('process/browser'),
    },
  },
};
