import { setTimeout } from 'timers/promises';
import CleanCss from 'clean-css';
import fs from 'fs-extra';
import minify from 'minify';
import path from 'path';
import { fileURLToPath } from 'url';
import UglifyJS from 'uglify-js';
import axios from 'axios';
import shiki from 'shiki';
import yaml from 'js-yaml';
import rehypeFormat from 'rehype-format';
import rehypeRaw from 'rehype-raw'
import rehypeDocument from 'rehype-document';
import rehypeStringify from 'rehype-stringify';
import remarkFrontmatter from 'remark-frontmatter';
import remarkGfm from 'remark-gfm';
import remarkParse from 'remark-parse';
import remarkRehype from 'remark-rehype';
import remarkSlug from 'remark-slug';
import remarkToc from 'remark-toc';
import remarkDirective from 'remark-directive';
import { unified } from 'unified';
import { visit } from 'unist-util-visit';
import { JSDOM } from 'jsdom';

const INTERNAL = [ 'base.html', 'config.json', 'index.md' ];

const normalizeDir = (s) => {
  return s.endsWith('/') ? s.slice(0, -1) : s;
}

const __filename = fileURLToPath(import.meta.url);
const rootDir = path.dirname(__filename);
const reachRoot = `${rootDir}/../../`;
const cfgFile = "config.json";
const repoBaseNice = "https://github.com/reach-sh/reach-lang/tree/master";
const repoBase = "https://raw.githubusercontent.com/reach-sh/reach-lang/master";
const repoSrcDir = "/docs/md/";
const srcDir = normalizeDir(`${rootDir}/src`);
const outDir = normalizeDir(`${rootDir}/build`);

// Plugins

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

    /*
    node.children = [
      ...node.children.slice(0, result.index),
      result.map,
      ...node.children.slice(result.endIndex)
    ]
    */
  }
};

const joinCodeClasses = () => {
  return (tree, file) => {
    visit(tree, 'code', node => {
      if(node.lang || node.meta) {
        node.lang =
          node.meta == null ? node.lang
          : node.lang == null ? node.meta.split(' ').join('_')
          : `${node.lang}_${node.meta.split(' ').join('_')}`;
      }
    });
  }
};

const copyFmToConfig = (configJson) => {
  return (tree, file) => {
    visit(tree, 'yaml', (node, index, p) => {
      const fm = yaml.load(node.value, 'utf8');
      for ( const k in fm ) {
        configJson[k] = fm[k];
      }
      p.children.splice(index, 1); // Remove yaml node.
      return [visit.SKIP, index];
    });
  }
};

const XXX = (name) => (...args) => {
  const m = ['XXX', name, ...args];
  console.log(m);
  return JSON.stringify(m);
};

const seclink = XXX('seclink');
const defn = XXX('defn');

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

const ref = XXX('ref');

const directive_note = (node) => {
  const data = node.data;
  data.hName = "div";
  data.hProperties = { class: "note" };
}
const directive_testQ = (node) => {
  const data = node.data;
  data.hName = "testQ";
  console.log(['XXX', 'testQ']);
}
const directive_testA = (node) => {
  const data = node.data;
  data.hName = "testA";
  console.log(['XXX', 'testA']);
}

const expanderEnv = { seclink, defn, workshopDeps, workshopInit, workshopWIP, errver, ref, directive_note, directive_testQ, directive_testA };

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
        expanderEnv[k](node);
      } else {
        console.log(['XXX expanderDirective', node.name]);
      }
    }
  })
}

// Tools
const writeFileMkdir = async (p, c) => {
  const dir = path.dirname(p);
  await fs.mkdir(dir, {recursive: true});
  await fs.writeFile(p, c);
};

const remoteGet_ = async (url) => {
  if ( url.startsWith(repoBase) ) {
    const n = url.replace(repoBase, reachRoot);
    return await fs.readFile(n, 'utf8');
  }
  console.log(`Downloading ${url}`);
  return (await axios.get(url)).data;
};
const CACHE = {};
const remoteGet = async (url) => {
  if ( ! (url in CACHE) ) {
    CACHE[url] = await remoteGet_(url);
  }
  return CACHE[url];
};

const shikiHighlighter =
  await shiki.getHighlighter({
    theme: 'github-light',
    langs: [
      ...shiki.BUNDLED_LANGUAGES,
      {
        id: 'reach',
        scopeName: 'source.js',
        // XXX customize this
        path: 'languages/javascript.tmLanguage.json',
      },
    ],
  });
const shikiHighlight = async (code, lang) => {
  return shikiHighlighter.codeToHtml(code, lang);
};

// Library
const cleanCss = new CleanCss({level: 2});
const processCss = async () => {
  const iPath = `${rootDir}/assets.in/styles.css`;
  const oPath = `${outDir}/assets/styles.min.css`;
  console.log(`Minifying ${iPath}`);
  const input = await fs.readFile(iPath, 'utf8');
  const output = cleanCss.minify(input);
  await writeFileMkdir(oPath, output.styles);
};

const processBaseHtml = async () => {
  const iPath = `${srcDir}/base.html`;
  const oPath = `${outDir}/base.html`;
  console.log(`Minifying ${iPath}`);
  const output = await minify(iPath, { html: {} });
  await writeFileMkdir(oPath, output);
};

const processJs = async () => {
  const iPath = `${rootDir}/assets.in/scripts.js`;
  const oPath = `${outDir}/assets/scripts.min.js`;
  console.log(`Minifying ${iPath}`);
  const input = await fs.readFile(iPath, 'utf8');
  const output = new UglifyJS.minify(input, {});
  if (output.error) throw output.error;
  await writeFileMkdir(oPath, output.code);
}

const AsyncFunction = Object.getPrototypeOf(async function(){}).constructor;
const processFolder = async ({baseConfig, relDir, in_folder, out_folder}) => {
  const lang = relDir.split('/')[0];
  const mdPath = `${in_folder}/index.md`;
  const cfgPath = `${out_folder}/${cfgFile}`;
  const pagePath = `${out_folder}/page.html`;
  const otpPath = `${out_folder}/otp.html`;

  console.log(`Building page ${relDir}`);

  // Create fresh config file with default values.
  const configJson = {
    ...baseConfig,
    chapters: null,
    pages: null,
  };

  const expandEnv = { ...configJson, ...expanderEnv };
  const expandKeys = Object.keys(expandEnv);
  const expandVals = Object.values(expandEnv);
  const evil = async (c) => {
    const af = new AsyncFunction(...expandKeys, `"use strict"; return (${c});`);
    try {
      return await af(...expandVals);
    } catch (e) {
      const es = `${e}`;
      console.log(`evil`, mdPath, c, `err`, es);
      return es;
    }
  }
  const expand = async (s) => {
    const ms = s.indexOf('@{'); // }
    if ( ms === -1 ) { return s; }
    /* { */ const me = s.indexOf('}', ms);
    if ( me === -1 ) { throw Error(`No closing } in interpolation: ${mdPath}: ${s.slice(ms)}`); }
    const pre = s.slice(0, ms);
    const mid = s.slice(ms+2, me);
    const pos = s.slice(me+1);
    const mid_e = await evil(mid);
    const posp = `${mid_e}${pos}`;
    const posp_e = await expand(posp);
    return `${pre}${posp_e}`;
  };

  const raw = await fs.readFile(mdPath, 'utf8');
  let md = await expand(raw);

  // markdown-to-html pipeline.
  const output = await unified()
    // Parse markdown to Markdown Abstract Syntax Tree (MDAST).
    .use(remarkParse)
    // Prepend YAML node with frontmatter.
    .use(remarkFrontmatter)
    // Remove YAML node and write frontmatter to config file.
    .use(copyFmToConfig, configJson)
    // Prepend Heading, level 6, value "toc".
    .use(prependTocNode)
    .use(remarkDirective)
    .use(expanderDirective)
    // Build toc list under the heading.
    .use(remarkToc, { maxDepth: 2 })
    // Create IDs (acting as anchors) for headings throughout the document.
    .use(remarkSlug)
    // Concatenate (using _) class names for code elements.
    .use(joinCodeClasses)
    // Normalize Github Flavored Markdown so it can be converted to html.
    .use(remarkGfm)
    // Convert MDAST to html.
    .use(remarkRehype, { allowDangerousHtml: true })
    // Copy over html embedded in markdown.
    .use(rehypeRaw)
    // Prettify html.
    .use(rehypeFormat)
    // Serialize html.
    .use(rehypeStringify)
    // Push the markdown through the pipeline.
    .process(md);

  const doc = new JSDOM(output).window.document;

  // Process OTP.
  if (doc.getElementById('toc')) {
    doc.getElementById('toc').remove();
    const otpEl = doc.querySelector('ul');
    otpEl.querySelectorAll('li').forEach(el => el.classList.add('dynamic'));
    otpEl.querySelectorAll('ul').forEach(el => el.classList.add('dynamic'));
    otpEl.querySelectorAll('p').forEach(el => {
      const p = el.parentNode;
      while (el.firstChild) { p.insertBefore(el.firstChild, el); }
      p.removeChild(el);
    })
    await fs.writeFile(otpPath, `<ul>${otpEl.innerHTML.trim()}</ul>`);
    otpEl.remove();
  }

  // Update config.json with title and pathname.
  const title = doc.querySelector('h1').textContent;
  doc.querySelector('h1').remove();
  configJson.title = title;
  configJson.pathname = in_folder;

  // Adjust image urls.
  doc.querySelectorAll('img').forEach(img => {
    if ( ! img.src.startsWith("/") ) {
      img.src = `/${relDir}/${img.src}`;
    }
  });

  // Process code snippets.
  const preArray = doc.querySelectorAll('pre');
  for (let i = 0; i < preArray.length; i++) {
    const pre = preArray[i];
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
        const url = line1.slice(5);
        if (url.slice(0, 4) == 'http') { spec.url = url; }
        else { spec.url = `${repoBase}${url}`; }
        if (arr.length > 1) {
          const line2 = arr[1].replace(/\s+/g, '');
          if (line2.slice(0, 6) == 'range:') {
            spec.range = line2.slice(6);
          }
        }
      }
    } }

    // Get remote content if specified.
    if (spec.url) {
      code.textContent = await remoteGet(spec.url);
    }

    code.textContent = code.textContent.trimEnd();

    // Highlight the content if specified.
    // https://github.com/shikijs/shiki/blob/main/docs/themes.md
    // https://github.com/shikijs/shiki/blob/main/docs/languages.md
    if (spec.language) {
      const hicode = await shikiHighlight(code.textContent, spec.language);
      code.textContent = hicode
        //.replace('<pre class="shiki" style="background-color: #282A36"><code>', '') // dracula
        .replace('<pre class="shiki" style="background-color: #ffffff"><code>', '') // github-light
        .replaceAll('<span class="line">', '')
        .replaceAll('</span></span>', '</span>')
        .replace('</code></pre>', '');
    }

    let firstLineIndex = null;
    let lastLineIndex = null;
    if (spec.range) {
      const rangeArr = spec.range.split('-');
      firstLineIndex = rangeArr[0] - 1;
      if (rangeArr.length > 1) {
        lastLineIndex = rangeArr[1] - 1;
      }
    }

    const arr = code.textContent.split(/\r?\n/g);
    let olStr = '<ol class="snippet">';
    for (let i = 0; i < arr.length; i++) {
      if (firstLineIndex && i < firstLineIndex) {
        continue;
      } else if (lastLineIndex && lastLineIndex < i) {
        break;
      }
      olStr += `<li value="${i + 1}">${arr[i]}</li>`;
    }
    olStr += '</ol>';
    code.remove();
    const olEl = doc.createRange().createContextualFragment(olStr);
    pre.append(olEl);
    pre.classList.add('snippet');
    const shouldNumber = spec.numbered && (arr.length != 1);
    pre.classList.add(shouldNumber ? 'numbered' : 'unnumbered');
  }

  // Write files
  await Promise.all([
    fs.writeFile(cfgPath, JSON.stringify(configJson, null, 2)),
    fs.writeFile(pagePath, doc.body.innerHTML.trim()),
  ]);
};

const findAndProcessFolder = async (base_html, inputBaseConfig, folder) => {
  let relDir = normalizeDir(folder.replace(srcDir, ''));
  relDir = relDir.startsWith('/') ? relDir.slice(1) : relDir;
  const in_folder = `${srcDir}/${relDir}`;

  const thisConfigP = `${in_folder}/${cfgFile}`;
  const thisConfig = (await fs.exists(thisConfigP)) ? (await fs.readJson(thisConfigP)) : {};
  const baseConfig = {
    ...inputBaseConfig,
    ...thisConfig,
  };

  if ( thisConfig.bookTitle !== undefined ) {
    console.log(`Found book ${relDir}`);
    baseConfig.bookPath = relDir;
  }

  const out_folder = `${outDir}/${relDir}`;
  await fs.mkdir(out_folder, { recursive: true })

  try { await fs.unlink(`${out_folder}/index.html`); } catch (e) { void(e); }
  await fs.symlink(base_html, `${out_folder}/index.html`);

  const fileArr = await fs.readdir(folder);
  await Promise.all(fileArr.map(async (p) => {
    if ( p === 'index.md' ) {
      return await processFolder({baseConfig, relDir, in_folder, out_folder});
    } else {
      const absolute = path.join(folder, p);
      const s = await fs.stat(absolute);
      if (s.isDirectory()) {
        return await findAndProcessFolder(`../${base_html}`, baseConfig, absolute);
      } else if ( ! INTERNAL.includes(p) ) {
        return await fs.copyFile(path.join(in_folder, p), path.join(out_folder, p));
      }
    }
  }));
};

// Main

await Promise.all([
  processCss(),
  processJs(),
  processBaseHtml(),
  findAndProcessFolder(`base.html`, process.env, srcDir),
]);
