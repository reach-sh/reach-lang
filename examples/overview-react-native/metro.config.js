/**
 * Metro configuration for React Native
 * https://github.com/facebook/react-native
 *
 * @format
 */
 const {getDefaultConfig} = require('metro-config');
module.exports = {
  transformer: {
    getTransformOptions: async () => ({
      transform: {
        experimentalImportSupport: false,
        inlineRequires: true,
      },
    }),
  },
};

module.exports = (async () => {
  const {
    resolver: {sourceExts},
  } = await getDefaultConfig();
  return {resolver: {sourceExts: [...sourceExts, 'mjs']}};
})();