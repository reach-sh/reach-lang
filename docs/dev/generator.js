import { setTimeout } from 'timers/promises';
import CleanCss from 'clean-css';
import fs from 'fs-extra';
import * as htmlMinify from 'html-minifier';
import path from 'path';
import { fileURLToPath } from 'url';
import UglifyJS from 'uglify-js';
import shiki from 'shiki';
import yaml from 'js-yaml';
import rehypeFormat from 'rehype-format';
import rehypeRaw from 'rehype-raw'
import rehypeDocument from 'rehype-document';
import rehypeStringify from 'rehype-stringify';
import rehypeAutolinkHeadings from 'rehype-autolink-headings';
import remarkFrontmatter from 'remark-frontmatter';
import remarkGfm from 'remark-gfm';
import remarkParse from 'remark-parse';
import remarkRehype from 'remark-rehype';
import remarkSlug from 'remark-slug';
import remarkToc from 'remark-toc';
import remarkDirective from 'remark-directive';
import { h } from 'hastscript';
import { unified } from 'unified';
import { visit } from 'unist-util-visit';
import { JSDOM } from 'jsdom';
import githubSlugger from 'github-slugger';
const slugify = githubSlugger.slug;
const topDoc = new JSDOM("").window.document;

const hh = (x) => x === '' ? '/' : `/${x}/`;
const INTERNAL = [ 'config.json' ];

const normalizeDir = (x) => {
  const s = path.normalize(x);
  const y = s.endsWith('/') ? s.slice(0, -1) : s;
  return y === "." ? "" : y;
}

const __filename = fileURLToPath(import.meta.url);
const rootDir = path.dirname(__filename);
const reachRoot = `${rootDir}/../../`;
const repoBaseNice = "https://github.com/reach-sh/reach-lang/tree/master";
const srcDir = normalizeDir(`${rootDir}/../src`);
const outDir = normalizeDir(`${rootDir}/../build`);
let forReal = false;
let hasError = false;

const warn = (...args) => {
  if ( forReal ) { console.log(...args); }
};
const fail = (...args) => {
  warn(...args);
  if ( forReal ) { hasError = true; }
};


const languages = [
  ...shiki.BUNDLED_LANGUAGES.map((l) => {
    if ( l.id === 'javascript' ) {
      l.aliases.push('mjs');
    }
    return l;
  }),
  {
    id: 'reach',
    scopeName: 'source.js',
    // XXX customize this
    path: 'languages/javascript.tmLanguage.json',
    aliases: ['rsh'],
  },
];
const languageNormalizeDict = {};
for (const l of languages) {
  languageNormalizeDict[l.id] = l.id;
  for (const alias of (l.aliases || [])) {
    languageNormalizeDict[alias] = l.id;
  }
}
const documentScopes = ['h', 'term'];
const normalizeScope = (s) => {
  const ns = (documentScopes.includes(s) && s) || languageNormalizeDict[s];
  if (!ns) {
    fail(`Bad xref scope:`, s);
  }
  return ns;
};

const xrefs = {};
const freshXrefScopeDict = () => ({"xrefs": {}, "prefixes": {}});
const xrefPut = (s, t, v) => {
  const ns = normalizeScope(s);
  if ( ! xrefs[ns] ) { xrefs[ns] = freshXrefScopeDict(); }
  const e = xrefs[ns].xrefs[t];
  if ( e !== undefined ) {
    const es = JSON.stringify(e);
    const vs = JSON.stringify(v);
    if ( es !== vs ) {
      fail(`Duplicated xref`, s, t, e, v);
    }
  }
  const parts = t.split(".");
  for (let i = 1; i < parts.length; i++){
    const prefix = parts.slice(0,i).join(".");
    xrefs[ns].prefixes[prefix] = true;
  }
  xrefs[ns]["xrefs"][t] = v;
};
const xrefGetInfo = (xrefsOrPrefixes, s, t) => {
  const ns = normalizeScope(s);
  return (xrefs[ns] || freshXrefScopeDict())[xrefsOrPrefixes][t];
};
const xrefGetIsPrefix = (s, t) => xrefGetInfo("prefixes", s, t);
const xrefGetMaybe = (s, t) => xrefGetInfo("xrefs", s, t);
const xrefGet = (s, t) => {
  const r = xrefGetMaybe(s, t);
  if ( r === undefined ) {
    fail(`Missing xref:`, t);
    return { title: t, path: t };
  }
  return r;
};

const urlExtension = (url) => {
  return url.split(/[#?]/)[0].split('.').pop().trim();
};

const prependTocNode = (options = {}) => {
  return (tree, file) => {
    tree.children = [
      {
        "type": "heading",
        "depth": 6,
        "children": [
          {
            "type": "text",
            "value": "toc",
            "position": {
              "start": {
                "line": 1,
                "column": 8,
                "offset": 7
              },
              "end": {
                "line": 1,
                "column": 11,
                "offset": 10
              }
            }
          }
        ],
        "position": {
          "start": {
            "line": 1,
            "column": 1,
            "offset": 0
          },
          "end": {
            "line": 1,
            "column": 12,
            "offset": 11
          }
        }
      },
      ...tree.children.slice(0)
    ];
  }
};

const copyFmToConfig = (configJson) => {
  return (tree, file) => {
    visit(tree, 'yaml', (node, index, p) => {
      const fm = yaml.load(node.value, 'utf8');
      for ( const k in fm ) {
        configJson[k] = fm[k];
      }
      // Remove yaml node.
      p.children.splice(index, 1);
      return [visit.SKIP, index];
    });
  }
};

// .use(inspect("beforeToc"), { here, what: "tut/rps" })
const inspect = (when) => ({here, what}) => (tree) => {
  if ( here.includes(what) ) {
    console.log(`inspect`, when, JSON.stringify(tree));
  }
};

const processXRefs = ({here}) => (tree) => {
  const h = hh(here);
  visit(tree, (node) => {
    const d = node.data || (node.data = {});
    const hp = d.hProperties || (d.hProperties = {});
    if ( node.type === 'heading' ) {
      const cs = node.children;
      if ( cs.length === 0 ) { return; }
      const c0v = cs[0].value;

      let v = c0v;
      let t = slugify(cs.map((x) => x.value).join(' '));
      if ( c0v && c0v.startsWith("{#") ) {
        const cp = c0v.indexOf("} ", 2);
        t = c0v.slice(2, cp);
        v = c0v.slice(cp+2);
        xrefPut('h', t, { title: v, path: `${h}#${t}` });
      }
      if ( t === 'on-this-page' ) {
        fail(here, 'uses reserved id', t);
      }

      d.id = hp.id = t;
      cs[0].value = v;
      if ( hp.class === undefined ) { hp.class = ''; }
      hp.class = `${hp.class} refHeader`;
    } else if ( node.type === 'link' ) {
      const u = node.url;
      if ( u && u.startsWith("##") ) {
        node.url = xrefGet('h', u.slice(2)).path;
      }
    }
  });
};

// Tools
const writeFileMkdir = async (p, c) => {
  const dir = path.dirname(p);
  await fs.mkdir(dir, {recursive: true});
  await fs.writeFile(p, c);
};

const sher =
  await shiki.getHighlighter({
    theme: 'github-light',
    langs: languages,
  });
const shikiHighlight = async (code, lang) => {
  const fc = sher.codeToHtml(code, lang);
  return fc
    .replace('<pre class="shiki" style="background-color: #ffffff"><code>', '')
    .replaceAll('<span class="line">', '')
    .replaceAll('</span></span>', '</span>')
    .replace('</code></pre>', '');
};

// Library
const cleanCss = new CleanCss({level: 2});
const processCss = async ({iPath, oPath}) => {
  const input = await fs.readFile(iPath, 'utf8');
  const output = cleanCss.minify(input);
  await writeFileMkdir(oPath, output.styles);
};

const processHtml = async ({iPath, oPath}) => {
  const defaultOptions = {
    removeComments: true,
    removeCommentsFromCDATA: true,
    removeCDATASectionsFromCDATA: true,
    collapseWhitespace: true,
    collapseBooleanAttributes: true,
    removeAttributeQuotes: true,
    removeRedundantAttributes: true,
    useShortDoctype: true,
    removeEmptyAttributes: true,
    removeEmptyElements: false,
    removeOptionalTags: true,
    removeScriptTypeAttributes: true,
    removeStyleLinkTypeAttributes: true,
    minifyJS: true,
    minifyCSS: true,
  };
  const xrefHref = (t) => {
    const { path } = xrefGet('h', t);
    return path;
  };
  const menuItem = (t, gtext = false) => {
    const { title, path } = xrefGet('h', t);
    const text = gtext || title;
    return `<a class="nav-link follow" href="${path}">${text}</a>`;
  };

  const raw = await fs.readFile(iPath, 'utf8');
  const expand = makeExpander('baseHtml', { menuItem, xrefHref });
  const exp = await expand(raw)
  const output = await htmlMinify.minify(exp, defaultOptions);
  await writeFileMkdir(oPath, output);
};

const processJs = async ({iPath, oPath}) => {
  const input = await fs.readFile(iPath, 'utf8');
  const output = false ? { code: input } : new UglifyJS.minify(input, {});
  if (output.error) throw output.error;
  await writeFileMkdir(oPath, output.code);
}

const AsyncFunction = Object.getPrototypeOf(async function(){}).constructor;

const makeExpander = (msg, expandEnv) => {
  const expandKeys = Object.keys(expandEnv);
  const expandVals = Object.values(expandEnv);
  const evil = async (c) => {
    const af = new AsyncFunction(...expandKeys, `"use strict"; return (${c});`);
    try {
      return await af(...expandVals);
    } catch (e) {
      fail(`evil`, msg, c, `err`, e);
      return `${e}`;
    }
  }
  const expand = async (s) => {
    const ms = s.indexOf('@{'); // }
    if ( ms === -1 ) { return s; }
    /* { */ const me = s.indexOf('}', ms);
    if ( me === -1 ) { throw Error(`No closing } in interpolation: ${msg}: ${s.slice(ms)}`); }
    const pre = s.slice(0, ms);
    const mid = s.slice(ms+2, me);
    const pos = s.slice(me+1);
    const mid_e = await evil(mid);
    const posp = `${mid_e}${pos}`;
    const posp_e = await expand(posp);
    return `${pre}${posp_e}`;
  };

  return expand;
};

const processMd = async ({baseConfig, relDir, in_folder, iPath, oPath}) => {
  const here = relDir;
  const h = hh(here);

  // Create fresh config file with default values.
  const configJson = {
    ...baseConfig,
    chapters: null,
    pages: null,
  };

  const seclink = (t) => {
    const { path, title } = xrefGet('h', t);
    return `[${title}](${path})`;
  };

  const workshopDeps = (pre) => {
    if ( pre === undefined ) {
      return `:::note\nThis workshop is independent of all others.\n:::\n`;
    } else {
      return `:::note\nThis workshop assumes that you have recently completed @{seclink(\"${pre}\")}.\n:::\n`;
    }
  };

  const workshopInit = (which) => {
    const s = [
      `We assume that you'll go through this workshop in a directory named \`~/reach/${which}\`:`,
      '```',
      `$ mkdir -p ~/reach/${which} && cd ~/reach/${which}`,
      '```',
      `And that you have a copy of Reach [installed](##ref-install) in \`~/reach\` so you can write`,
      '```',
      `$ ../reach version`,
      '```',
      `And it will run Reach.`,
      `You should start off by initializing your Reach program:`,
      '```',
      `$ ../reach init`,
      '```',
    ];
    return s.join('\n');
  };

  const workshopWIP = (dir) => {
    const d = `examples/${dir}`;
    const more = dir ? `If you'd like to see a draft version of our code, please visit [\`${d}\`](${repoBaseNice}/${d}).\n` : '';
    return `:::note\nThis page is a placeholder for a future more detailed workshop.\nYou could try to implement it yourself though, given the sketch above!\n${more}:::\n`;
  };

  const errver = (f, t) => {
    const m = (s, x) => `${s} version \`${x}\``;
    const fm = m('before', f);
    const tm = m('after', t);
    const msg = (f && t) ? `${fm} or ${tm}` : (f ? fm : tm);
    return `:::note\nThis error will not happen ${msg}.\n:::`;
  };

  const defn = (phrase) => {
    const t = encodeURI(`term_${phrase}`);
    xrefPut('term', phrase, {
      title: `Term: ${phrase}`,
      path: `${h}#${t}`,
    });
    return `<span class="term" id="${t}">${phrase}</span>`;
  };

  const ref = (scope, symbol) => {
    const t = encodeURI(`${scope}_${symbol}`);
    xrefPut(scope, symbol, {
      title: `${scope}: ${symbol}`,
      path: `${h}#${t}`,
    });
    const s = topDoc.createElement('span');
    s.classList.add("ref");
    s.id = t;
    s.setAttribute("data-scope", scope);
    s.setAttribute("data-symbol", symbol);
    return s.outerHTML;
  };

  const directive_note = (node) => {
    const data = node.data;
    data.hName = "div";
    data.hProperties = { class: "note" };
  };
  let c_qna = 0;
  const directive_testQ = (node) => {
    const data = node.data;
    const which = c_qna++;
    data.hName = "div";
    data.hProperties = {
      class: "q-and-a",
      "data-bs-toggle": "collapse",
      "aria-expanded": "false",
      href: `#q${which}`,
    };

    const pushAll = (n) => {
      const d = n.data || (n.data = {});
      d.which = which;
    };
    node.children.forEach(pushAll);
  };
  const directive_testA = (node) => {
    const data = node.data;
    const which = data.which;
    data.hName = "div";
    data.hProperties = {
      class: "collapse",
      id: `q${which}`,
    };
  };

  const generateIndex = () => {
    const r = [];
    for ( const s in xrefs ) {
      for ( const t in xrefs[s].xrefs ) {
        const { title, path } = xrefs[s].xrefs[t];
        r.push(`1. [${title}](${path})`);
      }
    }
    return r.join(`\n`);
  };

  const expanderEnv = { seclink, defn, workshopDeps, workshopInit, workshopWIP, errver, ref, directive_note, directive_testQ, directive_testA, generateIndex };

  const expanderDirective = () => (tree) => {
    visit(tree, (node) => {
      if (
        node.type === 'textDirective' ||
        node.type === 'leafDirective' ||
        node.type === 'containerDirective'
      ) {
        const data = node.data || (node.data = {});
        const k = `directive_${node.name}`;
        if (k in expanderEnv) {
          try { expanderEnv[k](node); }
          catch (e) {
            fail('expanderDirective', k, 'err', e);
          }
        } else {
          fail('expanderDirective', node.name, 'missing');
        }
      }
    })
  };

  let paraN = 0;
  const addParalinks = () => (tree) => {
    visit(tree, (node) => {
      if ( node.type === 'paragraph' ) {
        const tcs = node.children.filter((x) => x.type === 'text');
        if ( tcs.length > 0 ) {
          const i = paraN++;
          const is = i.toString();
          const id = `p_${is}`;
          node.children.unshift({type: 'html', value: `<i id="${id}" class="pid"></i>`});
          node.children.push({type: 'html', value: `<a href="#${id}" class="pid">${is}</a>`});
        }
      }
    })
  };

  const expand = makeExpander(iPath, { ...configJson, ...expanderEnv });

  const raw = await fs.readFile(iPath, 'utf8');
  let md = await expand(raw);

  const output = await unified()
    .use(remarkParse)
    // Prepend YAML node with frontmatter.
    .use(remarkFrontmatter)
    // Remove YAML node and write frontmatter to config file.
    .use(copyFmToConfig, configJson)
    .use(remarkDirective)
    .use(expanderDirective)
    .use(processXRefs, { here } )
    .use(addParalinks)
    // Prepend Heading, level 6, value "toc".
    .use(prependTocNode)
    // Build toc list under the heading.
    .use(remarkToc, { maxDepth: 100 })
    // Create IDs (acting as anchors) for headings throughout the document.
    .use(remarkSlug)
    // Normalize Github Flavored Markdown so it can be converted to html.
    .use(remarkGfm)
    .use(remarkRehype, { allowDangerousHtml: true })
    .use(rehypeRaw)
    .use(rehypeAutolinkHeadings, {
      behavior: 'append',
    })
    .use(rehypeFormat)
    .use(rehypeStringify)
    .process(md);

  const doc = new JSDOM(output).window.document;

  // Process OTP.
  let otpVal = undefined;
  const tocEl = doc.getElementById('toc');
  if (tocEl) {
    tocEl.remove();
    const otpEl = doc.querySelector('ul');
    otpEl.querySelectorAll('li, ul').forEach(el => el.classList.add('dynamic'));
    otpEl.querySelectorAll('p').forEach(el => {
      const p = el.parentNode;
      while (el.firstChild) { p.insertBefore(el.firstChild, el); }
      p.removeChild(el);
    })
    otpVal = `<ul>${otpEl.innerHTML.trim()}</ul>`;
    otpEl.remove();
  }

  // Update config.json with title and pathname.
  const theader = doc.querySelector('h1');
  const title = theader.textContent;
  configJson.title = title;
  configJson.titleId = theader.id;
  configJson.pathname = in_folder;

  if ( !forReal ) {
    const bp = configJson.bookPath;
    if ( bp !== undefined ) {
      const ib = bp === here;
      bookL.push({ here,
        title: (ib ? configJson.bookTitle : title),
        isBook: ib,
        rank: (configJson.bookRank || 0)});
    }
    return;
  }

  // Adjust image urls.
  doc.querySelectorAll('img').forEach(img => {
    if ( ! img.src.startsWith("/") ) {
      img.src = `${h}${img.src}`;
    }
  });

  await gatherSearchData({doc, title, here});
  theader.remove();

  // Process code snippets.
  const linkifySpans = (elem, language) => {
    const activePrefixes = [];
    const linkifySpan = (s, language) => {
      const text = s.textContent;
      const prefixedTexts = activePrefixes.map((p) => p + "." + text);
      prefixedTexts.push(text);
      const refs = prefixedTexts.map((t) => xrefGetMaybe(language, t));
      const ref = refs.find((r) => r)
      if (ref !== undefined) {
        const a = doc.createElement('a');
        a.href = ref.path;
        a.title = ref.title;
        a.innerHTML = s.outerHTML;
        s.outerHTML = a.outerHTML;
      }
      for (const t of prefixedTexts) {
        if (xrefGetIsPrefix(language, t) && ! activePrefixes.includes(t)) {
          activePrefixes.push(t);
        }
      }

    }
    for (const s of elem.querySelectorAll('span')) {
      linkifySpan(s, language);
    }
  }
  const mkEl = (s) => doc.createRange().createContextualFragment(s);
  for (const pre of doc.querySelectorAll('pre') ) {
    const code = pre.querySelector('code');
    if (!code) { continue; }

    const spec = {
      "numbered": true,
      "language": null, // indicates highlighted
      "url": null,      // indicates loaded
      "range": null     // indicates ranged
    };

    if (code.classList && code.classList.length == 1 && code.classList[0].startsWith('language')) {
      const arrLC = code.classList[0].replace('language-', '').split('_');
      for (let i = 0; i < arrLC.length; i++) {
        if (arrLC[i] == 'unnumbered' || arrLC[i] == 'nonum') {
          spec.numbered = false;
        } else {
          spec.language = arrLC[i];
        }
      }
    }

    { const arr = code.textContent.trimEnd().split(/\r?\n/g);
    if (arr.length > 0) {
      const line1 = arr[0].replace(/\s+/g, '');
      if (line1.slice(0, 5) == 'load:') {
        spec.load = line1.slice(5);
        if (arr.length > 1) {
          const line2 = arr[1].replace(/\s+/g, '');
          if (line2.slice(0, 6) == 'range:') {
            spec.range = line2.slice(6);
          }
        }
      }
    } }

    // Get remote content if specified.
    if (spec.load) {
      code.textContent = await fs.readFile(`${reachRoot}${spec.load}`, 'utf8');
      spec.language = urlExtension(spec.load);
    }

    code.textContent = code.textContent.trimEnd();
    let rawCode = code.textContent;

    // Highlight the content if specified.
    // https://github.com/shikijs/shiki/blob/main/docs/themes.md
    // https://github.com/shikijs/shiki/blob/main/docs/languages.md
    if (spec.language) {
      code.textContent = await shikiHighlight(rawCode, spec.language);
    }

    const splitRange = (input, f) => {
      const in_arr = input.split(/\r?\n/g);
      let firstLineIndex = null;
      let lastLineIndex = null;
      if (spec.range) {
        const rangeArr = spec.range.split('-');
        firstLineIndex = rangeArr[0] - 1;
        if (rangeArr.length > 1) {
          lastLineIndex = rangeArr[1] - 1;
        }
      }
      const out_arr = [];
      let i = 0;
      for ( const l of in_arr ) {
        if (firstLineIndex && i < firstLineIndex) {
          i++;
          continue;
        } else if (lastLineIndex && lastLineIndex < i) {
          break;
        }
        i++;
        out_arr.push(f(i, l));
      }
      return [ out_arr.join(''), in_arr.length != 1 ];
    };

    const [ olStrMid, hasSome ] =
      splitRange(
        code.textContent,
        (i, l) => `<li value="${i}">${l}</li>`)
    const olStr = `<ol class="snippet">${olStrMid}</ol>`;
    code.remove();
    const chEl = doc.createElement('div');
    chEl.classList.add("codeHeader");
    if ( spec.load ) {
      chEl.appendChild(mkEl(`<a href="${repoBaseNice}${spec.load}">${spec.load}</a>`));
    } else {
      chEl.appendChild(mkEl(`&nbsp;`));
    }
    const cpEl = doc.createElement('a');
    cpEl.classList.add("far", "fa-copy", "copyBtn");
    const copyCode = ( spec.language === 'cmd' ) ?
      rawCode.replace(/^\$ /, '') : rawCode;
    cpEl.setAttribute("data-clipboard-text",
      splitRange(copyCode, (i, l) => `${l}\n`)[0].trimEnd());
    cpEl.href = "#";
    chEl.appendChild(cpEl);
    pre.append(chEl);
    pre.append(mkEl(olStr));
    pre.classList.add('snippet');
    const shouldNumber = spec.numbered && hasSome;
    pre.classList.add(shouldNumber ? 'numbered' : 'unnumbered');
    linkifySpans(pre, spec.language);
  }
  for (const c of doc.querySelectorAll('code') ) {
    const rt = c.textContent.trimEnd();
    if ( ! rt.startsWith('{!') ) { continue; }
    const le = rt.indexOf('} ');
    if ( ! le ) { continue; }
    const lang = rt.slice(2, le);
    const rc = rt.slice(le+2);
    const hc = await shikiHighlight(rc, lang);
    const s = doc.createElement('span');
    s.classList.add('snip');
    s.appendChild(mkEl(hc));
    linkifySpans(s, lang);
    c.outerHTML = s.outerHTML;
  }

  // Write files
  const configJsonSaved = {};
  for ( const k of ['bookPath', 'title', 'titleId', 'hasOtp', 'hasPageHeader'] ) {
    configJsonSaved[k] = configJson[k];
  }

  const cfgVal = configJsonSaved;
  const pageVal = doc.body.innerHTML.trim();
  await fs.writeFile(oPath, JSON.stringify([ cfgVal, pageVal, otpVal ]));
};

const splitMt = (x) => x === "" ? [] : x.split('/');

const bookL = [];
const bookT = { path: [], children: {} };
const generateBooksTree = () => {
  for ( const c of bookL ) {
    let ct = bookT;
    let cp = [];
    let p = splitMt(c.here);
    while ( p.length !== 0 ) {
      ct = ct.children;
      const [ n, ...pn ] = p;
      p = pn;
      cp = [...cp, n];
      if ( ! (n in ct) ) {
        ct[n] = { path: cp, children: {} };
      }
      ct = ct[n];
    }
    Object.assign(ct, c);
  }
};
const bookPipe = await unified()
  .use(rehypeStringify);
const generateBook = async (destp, bookp) => {
  //console.log(`generateBook`, JSON.stringify(bookp));
  const compareNumbers = (a, b) => (a - b);
  const compareChapters = (x, y) => {
    const rc = compareNumbers(x.rank, y.rank);
    if ( rc !== 0 ) { return rc; }
    return x.title.localeCompare(y.title);
  };
  const hify = (ctc) => {
    const d = h('div', {class: "row chapter dynamic"});
    const cs = [];
    if ( ctc.isBook || Object.keys(ctc.children).length === 0 ) {
      d.children.push(h('div', {class: "col-auto chapter-empty-col"}));
    } else {
      d.children.push(h('div', {class: "col-auto chapter-icon-col"}, [
        h('i', {class: "chapter-icon fas fa-angle-right"})
      ]));
      cs.push(h('div', {class: "pages", style: "display:none"},
        hifyList(ctc)
      ));
    }
    if ( ctc.title === undefined ) {
      fail(`Missing chapter title`, ctc.path);
    }
    d.children.push(h('div', {class: "col"}, [
      h('a', {class: "chapter-title", href: hh(ctc.here)}, ctc.title),
      ...cs
    ]));
    return d;
  };
  const hifyList = (ct) => {
    const cs = Object.values(ct.children || {});
    cs.sort(compareChapters);
    return cs.map(hify);
  }
  const hifyTop = (ct, p) => {
    //console.log(`hifyTop`, JSON.stringify(p));
    const bc = [];
    const bcPush = () => {
      bc.push(
        h('div', {class: "row chapter dynamic"}, [
          h('div', {class: "col-auto chapter-icon-col"}, [
            h('i', {class: "fas fa-angle-down"})
          ]),
          h('div', {class: "col my-auto"}, [
            h('a', {class: "book-title", href: hh(ct.here)}, ct.title)
          ]),
        ])
      );
    }
    bcPush();
    for ( const n of p ) {
      ct = ((ct.children || {})[n] || {});
      bcPush();
    }
    const bcd = h('div', {class: "bookCrumbs dynamic"}, bc);
    return [ bcd ].concat(hifyList(ct));
  };
  const toc = { type: 'root', children: [] };
  toc.children = hifyTop(bookT, splitMt(bookp));
  await fs.writeFile(destp, bookPipe.stringify(toc));
};

const processAll = async (base_html, inputBaseConfig, folder) => {
  let relDir = normalizeDir(folder.replace(srcDir, ''));
  relDir = relDir.startsWith('/') ? relDir.slice(1) : relDir;
  const in_folder = `${srcDir}/${relDir}`;

  const cfgFile = "config.json";
  const thisConfigP = `${in_folder}/${cfgFile}`;
  const thisConfig = (await fs.exists(thisConfigP)) ? (await fs.readJson(thisConfigP)) : {};
  const baseConfig = {
    ...inputBaseConfig,
    ...thisConfig,
    ignored: [],
  };
  const ignored = thisConfig.ignored || [];

  const out_folder = `${outDir}/${relDir}`;
  await fs.mkdir(out_folder, { recursive: true })

  try { await fs.unlink(`${out_folder}/index.html`); } catch (e) { void(e); }
  await fs.symlink(base_html, `${out_folder}/index.html`);

  if ( thisConfig.bookTitle !== undefined ) {
    baseConfig.bookPath = relDir;
    if ( ! await fs.exists(`${in_folder}/book.html`) ) {
      await generateBook(`${out_folder}/book.html`, relDir);
    }
  }

  const opts = {baseConfig, relDir, in_folder, out_folder};

  const fileArr = await fs.readdir(folder);
  await Promise.all(fileArr.map(async (p) => {
    const iPath = `${in_folder}/${p}`;
    const oPath = `${out_folder}/${p}`;
    const opts_p = { ...opts, iPath, oPath };
    const e = urlExtension(p);
    const absolute = path.join(folder, p);
    const s = await fs.stat(absolute);
    if (s.isDirectory()) {
      return await processAll(`../${base_html}`, baseConfig, absolute);
    } else if ( ignored.includes(p) ) {
      return;
    } else if ( e === 'md' ) {
      return await processMd(opts_p);
    } else if ( forReal ) {
      if ( e === "css" ) {
        return await processCss(opts_p);
      } else if ( e === "html" ) {
        return await processHtml(opts_p);
      } else if ( e === "js" ) {
        return await processJs(opts_p);
      } else if ( ! INTERNAL.includes(p) ) {
        return await fs.copyFile(path.join(in_folder, p), path.join(out_folder, p));
      }
    }
  }));
};

const generateRedirects = async () => {
  const rehtml = await fs.readFile('redirect.html', { encoding: 'utf8' });
  const ms = await fs.readFile('manifest.txt', { encoding: 'utf8' });
  const fl = ms.trimEnd().split('\n');
  await Promise.all(fl.map(async (f) => {
    const { path } = xrefGet('h', f);
    const expand = makeExpander('generateRedirects', { URL: path });
    const re_f = await expand(rehtml);
    await fs.writeFile( `${outDir}/${f}.html`, re_f );
  }));
};

const searchData = [];
const [ sd_r, sd_t, sd_h, sd_p ] = [ 0, 1, 2, 3 ];
const gatherSearchData = async ({doc, title, here}) => {
  const h = hh(here);
  const mini = (x) => x.replace(/\s+/g, ' ').trim();
  doc.querySelectorAll('.ref').forEach((el) => {
    searchData.push({
      objectID: `${h}#${el.id}`,
      pt: title,
      t: sd_r,
      s: el.getAttribute('data-scope'),
      c: el.getAttribute('data-symbol'),
    });
  });
  doc.querySelectorAll('.term').forEach((el) => {
    searchData.push({
      objectID: `${h}#${el.id}`,
      pt: title,
      t: sd_t,
      c: mini(el.textContent),
    });
  });
  doc.querySelectorAll('.refHeader').forEach((el) => {
    searchData.push({
      objectID: `${h}#${el.id}`,
      pt: title,
      t: sd_h,
      c: mini(el.textContent),
    });
  });
  doc.querySelectorAll('i.pid').forEach((el) => {
    // XXX it would be cool to get the closest header, but .closest won't do
    // it, because the header won't be a parent
    searchData.push({
      objectID: `${h}#${el.id}`,
      pt: title,
      t: sd_p,
      c: mini(el.parentElement.textContent).replace(/\d+$/, ''),
    });
  });
};
const generateSearch = async () => {
  await fs.writeFile(`${rootDir}/searchData.json`, JSON.stringify(searchData,null,2));
};

const doTop = () =>
  processAll(`base.html`, process.env, srcDir);

// Main
await doTop();
console.log(JSON.stringify(bookL, null, 2));
generateBooksTree();
console.log(JSON.stringify(bookT, null, 2));

forReal = true;
// This depends on the xrefs being assembled
await Promise.all([
  doTop(),
  generateRedirects(),
]);
// This depends on the actual content being complete
await Promise.all([
  generateSearch(),
]);

if ( hasError ) {
  throw Error(`Build had errors`);
}
