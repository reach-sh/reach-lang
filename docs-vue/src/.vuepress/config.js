const shiki = require('shiki');
const fs = require('fs');
const path = require('path');
const linkReplace = require('markdown-it-attr-link-replacer').default;

// Add Reach to Shiki
shiki.BUNDLED_LANGUAGES.forEach((o) => {
  if ( o.id === 'javascript' ) {
    o.aliases.push('mjs');
    // XXX remove
    o.aliases.push('reach', 'rsh');
  }
});
// XXX enable
if ( false ) {
shiki.BUNDLED_LANGUAGES.push({
  id: 'reach',
  scopeName: 'source.reach',
  path: 'reach.tmLanguage.json',
  aliases: [ 'rsh' ],
});
}

const safeJSONRead = (p) => {
  try {
    return JSON.parse(fs.readFileSync(p));
  } catch (e) {
    return {};
  }
};

const grPath = path.resolve(__dirname, 'globalRefs.json');
const globalRefs = safeJSONRead(grPath);
const addGlobalRef = (ref, tgt) => {
  const now = globalRefs[ref]
  if ( ! now ) {
    globalRefs[ref] = tgt;
    fs.writeFileSync(grPath, JSON.stringify(globalRefs, null, 2));

  } else if ( now !== tgt ) {
    console.log(`XXX '${ref}' already defined: '${now}', can't change to '${tgt}'`);
  }
};

const reachLink = (link) => {
  if ( link.startsWith('##') ) {
    const ref = link.substring(2);
    const tgt = globalRefs[ref];
    if ( tgt ) { return tgt; }
    console.log(`XXX Unknown globalRef: ${ref}`);
    return `XXX ${ref}`;
  } else if ( link.startsWith('@') ) {
    return link.replace(/^@github/, 'https://github.com/reach-sh/reach-lang/blob/master');
  } else {
    return link;
  }
};

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
        link: reachLink('##community') },
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
        const ep = path.resolve(__dirname, '../../../');
        return str.replace(/^@reach-lang/, ep);
      },
    },
  },
  extendsMarkdown: (md) => {
    // Register global refs
    md.core.ruler.before("linkify", "globalRefRegister", (state) => {
      const toks = state.tokens;
      toks.forEach((t, i) => {
        if ( t.type === 'heading_open' ) {
          const after = toks[i+1];
          if ( after.type === 'inline' && after.children.length > 0 ) {
            const data = after.children[0];
            const datac = data.content;
            if ( datac.startsWith('{#') ) {
              const end = datac.indexOf('}', 2);
              const ref = datac.substring(2, end);
              t.attrSet('id', ref);
              const tgt = state.env.filePathRelative + '#' + ref;
              addGlobalRef(ref, tgt);
              data.content = datac.substring(end+1);
            }
          }
        }
      });
    });

    // Link global refs and aliases
    linkReplace(md, {
      attributes: ['src', 'href'],
      callback: function (link, env) {
        return reachLink(link);
      },
    });
  },
  plugins: [
    [ '@vuepress/google-analytics', {
      id: "UA-149147406-2"
    } ],
    [ '@vuepress/plugin-shiki', {
      // XXX If you change this theme, then the background won't take effect,
      // because the default Vue theme's SCSS use var(--code-bg-color) to
      // override it AND to style the line numbers, etc.
      theme: 'nord',
      langs: [],
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
