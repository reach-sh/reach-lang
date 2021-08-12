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
    docsDir: 'docs-vue/src',
    editLinks: true,
    editLinkText: 'Help us improve this page!',
    searchPlaceholder: 'Search...',
    lastUpdated: 'Last Updated',
  },
  markdown: {
    lineNumbers: true,
    extendMarkdown: md => {
    }
  },
  plugins: [
    [ "@vuepress/google-analytics", {
      ga: "UA-149147406-2"
    } ],
    [ '@vuepress/active-header-links', {
      sidebarLinkSelector: '.sidebar-link',
      headerAnchorSelector: '.header-anchor'
    } ],
    [ '@vuepress/back-to-top', {} ],
    [ '@vuepress/blog', {} ],
  ],
  configureWebpack: {
    resolve: {
      alias: {
        '@examples': '../examples/'
      }
    }
  }
};
