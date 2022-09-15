const getSetObj = (obj, k, def) => {
  obj[k] = obj[k] || def;
  return obj[k];
};
module.exports = function override(config, env) {
  const resolve = getSetObj(config, 'resolve', {});
  const fallback = getSetObj(resolve, 'fallback', {});
  Object.assign(fallback, {
    crypto: require.resolve('crypto-browserify'),
    http: require.resolve('stream-http'),
    stream: require.resolve('stream-browserify'),
    process: require.resolve('process/browser'),
    path: require.resolve("path-browserify"),
  });
  return config;
};
