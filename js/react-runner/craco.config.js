const path = require('path');

module.exports = {
  webpack: {
    alias: {
      assert: path.resolve(__dirname, './node_modules/assert'),
      stream: path.resolve(__dirname, './node_modules/readable-stream'),
      buffer: path.resolve(__dirname, './node_modules/buffer'),
      util: path.resolve(__dirname, './node_modules/util'),
    },
  },
};
