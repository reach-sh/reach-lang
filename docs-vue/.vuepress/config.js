module.exports = {
  title: 'Reach',
  locales: {
    '/': {
      lang: 'en-US', // this will be set as the lang attribute on <html>
    },
  },
  configureWebpack: {
    resolve: {
      alias: {
        '@examples': '../examples/'
      }
    }
  }
}
