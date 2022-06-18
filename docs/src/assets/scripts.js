const axiosGetData = async (u) => {
  const c = await axios.get(u);
  return c.data;
};
const doc = window.document;
const hh = (x) => x === '' ? '/' : `/${x}/`;

const searchClient = algoliasearch('M53HHHS0ZW', '0cfd8f1c1a0e3cb7b2abd77b831614dc');
const searchIndex = searchClient.initIndex('docs');

const currentPage = {
  folder: null,
  bookPath: undefined,
  hasOtp: false,
};

// let lang = window.navigator.language.split('-')[0];

const getWinWidthStr = () => {
  const s = window.innerWidth;
  if (s >= 1200) { return 'xl' }
  else if (s >= 992) { return 'lg' }
  else if (s >= 768) { return 'md' }
  else if (s >= 576) { return 'sm' }
  else return 'xs'
}

const maxColWidth = '280px';
let winWidth = getWinWidthStr();

const establishDisplay = () => {
  const { bookPath, hasOtp } = currentPage;
  establishDisplayFor('book-col', 'div.show-book-col', bookPath);
  establishDisplayFor('otp-col', 'button.show-otp-col', hasOtp);
};

const establishDisplayFor = (id, selector, property) => {
  if (property === undefined || property === false) {
    return;
  }
  const element = doc.getElementById(id);
  const button = doc.querySelector(selector);
  if (winWidth == 'xl' || winWidth == 'lg' || winWidth == 'md') {
    element.style.maxWidth = maxColWidth;
    element.style.display = 'block';
    button.style.display = 'none';
  } else if (winWidth == 'sm' || winWidth == 'xs') {
    element.style.maxWidth = 'none';
    element.style.display = 'none';
    button.style.display = 'block';
  }

  const prev = localStorage.getItem(id);
  switch (prev) {
    case 'block':
      element.style.display = 'block';
      button.style.display = 'none';
      break;

    case 'none':
      element.style.display = 'none';
      button.style.display = 'block';
      break;

    default:
      const { display } = element.style;
      localStorage.setItem(id, display);
      break;
  }
};

window.addEventListener('resize', () => {
  const newWinWidth = getWinWidthStr();
  if (winWidth != newWinWidth) {
    winWidth = newWinWidth;
    establishDisplay();
  }
});

const scrollHandler = (event) => {
  if (doc.querySelectorAll('#otp-col li.dynamic').length == false) {
    event.target.onscroll = null;
  } else {
    const a = doc.createElement('a');
    const arr = doc.querySelectorAll('#otp-col a');
    let t = 'on-this-page';
    for (let i = arr.length - 1; i >= 0; i--) {
      a.href = arr[i].href;
      const h = a.hash;
      const hid = h && h.substring(1);
      const el = hid && doc.getElementById(hid);
      const rect = el && el.getBoundingClientRect();
      if (rect && rect.y <= 80.0) {
        t = el.id;
        break;
      }
    }
    gotoTarget(false, t, false);
  }
}

const scrollPage = (id) => {
  if (id == 'on-this-page') {
    doc.getElementById('page-col').scrollTo(0, 0);
  } else {
    const t = doc.getElementById(id);
    if ( t ) { t.scrollIntoView(); }
  }
};

const updateHistory = (id) => {
  const base = `${window.location.origin}${currentPage.folder}`;
  const p = (id == 'on-this-page') ? base : `${base}#${id}`;
  window.history.pushState(null, '', p);
}

const gotoTarget = async (shallUpdateHistory, t, shouldScroll = true) => {
  if ( shouldScroll ) { scrollPage(t); }
  if ( shallUpdateHistory ) { updateHistory(t); }
  setOtpItemToActive(t);
};

const setOtpItemToActive = (id) => {
  const a = doc.querySelector('#otp-col a.active');
  if ( a ) { a.classList.remove('active'); }
  const link =
    (id === 'on-this-page') ?
      doc.querySelector('#otp-col a[href="#on-this-page"]') :
      doc.querySelector('#otp-col a[href="' + "#" + id + '"]');
  if ( link && link.classList && link.classList.contains('active') === false) {
    link.classList.add('active');
  }
};

const getWebpage = async (folder, hash, shallUpdateHistory) => {
  folder = folder.replace(/index\.html$/, '');
  const url = `${window.location.origin}${folder}`;
  if ( ! folder || ! url ) { throw Error(`getWebpage on undefined`); }

  const [ configJson, pageHtml, otpHtml ] =
    await axiosGetData(`${url}index.md`);

  // Book or different book?
  if (configJson.bookPath !== undefined && configJson.bookPath !== currentPage.bookPath) {
    let bookHtml = doc.createRange().createContextualFragment(await axiosGetData(`${window.location.origin}${hh(configJson.bookPath)}book.html`));
    doc.querySelectorAll('#book-col div.dynamic').forEach(n => n.remove());
    doc.querySelector('#book-col').append(bookHtml);

    // On click chapter-icon.
    doc.querySelectorAll('#book-col i.chapter-icon').forEach(el => {
      el.addEventListener('click', (evt) => {
        const item = evt.target;
        const pages = item.closest('div.chapter').querySelector('div.pages');
        if (item.classList.contains('fa-angle-right')) {
          item.classList.remove('fa-angle-right');
          item.classList.add('fa-angle-down');
          pages.style.display = 'block';
        } else {
          item.classList.remove('fa-angle-down');
          item.classList.add('fa-angle-right');
          pages.style.display = 'none';
        }
      });
    });
  }
  currentPage.bookPath = configJson.bookPath;

  // Write page title
  const ctitle = configJson.title;
  const tspan = doc.querySelector('div#hh-viewer-wrapper span.title');
  tspan.id = configJson.titleId;
  tspan.textContent = ctitle;
  doc.title = `Reach > ${ctitle}`;

  // Update and show/hide edit btn.
  // XXX move into generator
  const github = 'https://github.com/reach-sh/reach-lang/tree/master/docs/src';
  doc.getElementById('edit-btn').href = `${github}${folder}index.md`;

  // Write page html
  const pageDoc = doc.createRange().createContextualFragment(pageHtml);
  doc.querySelector('div#hh-viewer-wrapper div#hh-viewer').textContent = '';
  doc.querySelector('div#hh-viewer-wrapper div#hh-viewer').append(pageDoc);

  // If search page.
  const searchInput = doc.getElementById('search-input');
  if (searchInput) {
    currentPage.bookPath = undefined;
    searchInput.focus();
    const searchResultsList = doc.getElementById('search-results-list');
    searchInput.addEventListener('keyup', async (evt) => {
      const { hits } = await searchIndex.search(searchInput.value);
      if ( ! hits.length ) { return; }
      searchResultsList.innerHTML = '';
      hits.forEach((hit) => {
        const sdClasses = [
          'sdRef',
          'sdTerm',
          'sdHeader',
          'sdPara',
          'sdGHDis',
        ];
        const c = sdClasses[hit.t];
        const e = doc.createElement('li');
        e.classList.add(c);
        const h = (cls, t) => {
          const n = doc.createElement('span');
          n.classList.add(cls);
          n.innerText = t;
          e.appendChild(n);
        };
        const a = doc.createElement('a');
        a.classList.add('pt');
        a.href = hit.objectID;
        a.innerText = hit.pt;
        e.appendChild(a);
        if ( c === 'sdRef' ) {
          h('symbol', hit.c);
          h('scope', hit.s);
        } else if ( c === 'sdTerm' ) {
          h('term', hit.c);
        } else if ( c === 'sdHeader' ) {
          h('h', hit.c);
        } else if ( c === 'sdPara' ) {
          h('p', hit.c);
        } else if ( c === 'sdGHDis' ) {
          h('p', hit.c);
        }
        searchResultsList.append(e);
      });
      setClickFollowLink();
    });
  }

  // Write otp html.
  if (configJson.hasOtp) {
    doc.querySelectorAll('#otp-col ul ul.dynamic, #otp-col ul li.dynamic').forEach(n => { n.remove(); });
    const otpUl = doc.querySelector('#otp-col ul');
    const otpDoc = doc.createRange().createContextualFragment(otpHtml);
    const oul = otpDoc.querySelector('ul');
    if ( oul ) {
      oul.querySelectorAll(':scope > li').forEach((el, index) => {
        if (index == 0) {
          const ul = el.querySelector('ul');
          if (ul) {
            otpUl.querySelector('li').append(ul);
          }
        } else {
          otpUl.append(el);
        }
      });
    }
  }
  currentPage.hasOtp = configJson.hasOtp;

  // Adjust active indicators.
  doc.querySelectorAll('a').forEach(el => {
    el.classList.remove('active');
  });

  doc.querySelectorAll(`a[href="${folder}"]`).forEach((el) => {
    el.classList.add('active');
  });

  const el = doc.querySelector(`#book-col a[href="${folder}"]`);
  if (el) {
    const chapter = el.closest('div.chapter');
    const pages = chapter && chapter.querySelector('div.pages');
    if (pages && pages.hasChildNodes()) {
      const icon = chapter.querySelector('i.chapter-icon');
      icon.classList.remove('fa-angle-right');
      icon.classList.add('fa-angle-down');
      pages.style.display = 'block';
    }
  }

  // Establish correct display values.
  establishDisplay();

  // Display book.
  if (currentPage.bookPath != undefined) {
    doc.getElementById('book-col').classList.remove('banish');
    doc.querySelector('div.show-book-col').classList.remove('banish');
  } else {
    doc.getElementById('book-col').classList.add('banish');
    doc.querySelector('div.show-book-col').classList.add('banish');
  }

  // Display page.
  doc.querySelector('div#hh-page-header').style.display = configJson.hasPageHeader ? 'block' : 'none';

  doc.getElementById('page-col').style.display = 'block';

  // Display OTP.
  if (configJson.hasOtp) {
    doc.getElementById('otp-col').classList.remove('banish');
    doc.querySelector('button.show-otp-col').classList.remove('banish');
  } else {
    doc.getElementById('otp-col').classList.add('banish');
    doc.querySelector('button.show-otp-col').classList.add('banish');
  }

  // Scroll to proper place and update history
  currentPage.folder = folder;
  setClickFollowLink();
  await gotoTarget(shallUpdateHistory, hash ? hash.substring(1) : 'on-this-page');
};

const clickFollowLink = async (evt) => {
  if ( evt.shiftKey || evt.ctrlKey || evt.metaKey ) { return; }
  const t = evt.target.closest('a');
  if ( t.classList && t.classList.contains("copyBtn") ) {
    evt.preventDefault();
    await navigator.clipboard.writeText(t.getAttribute('data-clipboard-text'));
    return;
  }
  const href = t.href;
  const a = doc.createElement('a');
  a.href = href;
  if (a.hostname === window.location.hostname) {
    evt.preventDefault();
    if (currentPage.folder == a.pathname && a.hash) {
      const t = (a.hash === '#on-this-page') ? 'on-this-page' : a.hash.substring(1);
      await gotoTarget(true, t);
    } else {
      await getWebpage(a.pathname, a.hash, true);
    }
  }
};

const setClickFollowLink = () => {
  doc.querySelectorAll('a').forEach((el) => {
    el.addEventListener('click', clickFollowLink);
  });
};

window.onpopstate = function (event) {
  const a = doc.createElement('a');
  a.href = doc.location.href;
  getWebpage(a.pathname, a.hash, false);
};

const makeShowHide = (hideQ, showQ, showId) => {
  const f = (isHide) => {
    const g = (b) => b ? 'block' : 'none';
    const x = g(isHide); const y = g(! isHide);
    const q = isHide ? hideQ : showQ;
    doc.querySelector(q).addEventListener('click', (event) => {
      if (winWidth == 'sm' || winWidth == 'xs') {
        doc.getElementById('page-col').style.display = x;
      }
      doc.getElementById(showId).style.display = y;
      doc.querySelector(showQ).style.display = x;
    });
  };
  f(true);
  f(false);
};

makeShowHide('button.hide-book-icon', 'div.show-book-col', 'book-col');
makeShowHide('button.hide-otp-icon', 'button.show-otp-col', 'otp-col');

doc.getElementById('page-col').addEventListener('scroll', scrollHandler);

getWebpage(window.location.pathname, window.location.hash, true);

