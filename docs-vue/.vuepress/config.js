module.exports = {
  title: 'Reach',
  locales: {
    '/': {
      lang: 'en-US', // this will be set as the lang attribute on <html>
    },
  },
  base: '/',
  themeConfig: {
    repo: 'reach-sh/reach-lang',
    repoLabel: 'GitHub',
    docsDir: 'docs-vue',
    editLinks: true,
    editLinkText: 'Help us improve this page!',
    searchPlaceholder: 'Search...',
    lastUpdated: 'Last Updated',
  },
  plugins: [
    [
      "@vuepress/google-analytics",
      {
        ga: "UA-149147406-2"
      }
    ],
  ],
  configureWebpack: {
    resolve: {
      alias: {
        '@examples': '../examples/'
      }
    }
  }
};
