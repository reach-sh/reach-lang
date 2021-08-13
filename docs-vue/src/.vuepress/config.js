const path = require('path');

module.exports = {
  bundler: '@vuepress/vite',
  lang: 'en-US',
  title: 'Reach',
  base: '/',
  themeConfig: {
    home: '/',
    repo: 'reach-sh/reach-lang',
    repoLabel: 'GitHub',
    docsDir: 'docs-vue/src',
    editLink: true,
    editLinkText: 'Help us improve this page!',
    searchPlaceholder: 'Search...',
    contributors: true,
    contributorsText: 'Authors',
    logo: '/reach-icon.svg',
    navbar: [
      { text: 'Discord',
        link: 'https://discord.gg/AZsgcXu' },
      { text: 'Community',
        link: '##community' },
    ],
    themePlugins: {
    },
  },
  markdown: {
    code: {
      lineNumbers: true,
    },
    importCode: {
      handleImportPath: (str) => {
        const ep = path.resolve(__dirname, '../../../examples/');
        return str.replace(/^@examples/, ep);
      },
    },
  },
  plugins: [
    [ '@vuepress/google-analytics', {
      id: "UA-149147406-2"
    } ],
    [ '@vuepress/plugin-search', {
    } ],
    [ '@vuepress/container', {
      type: 'note',
      locales: {
        '/': { defaultInfo: "Note" }
      },
    } ],
  ],
};
