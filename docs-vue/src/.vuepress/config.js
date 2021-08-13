module.exports = {
  lang: 'en-US',
  title: 'Reach',
  base: '/',
  themeConfig: {
    repo: 'reach-sh/reach-lang',
    repoLabel: 'GitHub',
    docsDir: 'docs-vue/src',
    editLink: true,
    editLinkText: 'Help us improve this page!',
    searchPlaceholder: 'Search...',
    lastUpdated: 'Last Updated',
    themePlugins: {
    },
  },
  markdown: {
    code: {
      lineNumbers: true,
    },
  },
  plugins: [
    [ '@vuepress/google-analytics', {
      id: "UA-149147406-2"
    } ],
    [ '@vuepress/plugin-search', {
    } ],
  ],
};
