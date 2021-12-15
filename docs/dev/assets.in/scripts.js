const axiosGetData = async (u) => {
  const c = await axios.get(u);
  return c.data;
};

import algoliasearch from 'https://cdn.jsdelivr.net/npm/algoliasearch@4/dist/algoliasearch-lite.esm.browser.js';
const searchClient = algoliasearch('M53HHHS0ZW', '0cfd8f1c1a0e3cb7b2abd77b831614dc');
const searchIndex = searchClient.initIndex('docs');

const currentPage = {
  folder: null,
  bookPath: null,
  hasOtp: false,
};

// XXX move into generator
const github = 'https://github.com/reach-sh/reach-lang/tree/master/docs/dev/src';

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
  if (currentPage.bookPath) {
    const bookCol = document.getElementById('book-col');
    const bookBtn = document.querySelector('div.show-book-col');
    if (winWidth == 'xl' || winWidth == 'lg' || winWidth == 'md') {
      bookCol.style.maxWidth = maxColWidth;
      bookCol.style.display = 'block';
      bookBtn.style.display = 'none';
    } else if (winWidth == 'sm' || winWidth == 'xs') {
      bookCol.style.maxWidth = 'none';
      bookCol.style.display = 'none';
      bookBtn.style.display = 'block';
    }
  }

  if (currentPage.hasOtp) {
    const otpCol = document.getElementById('otp-col');
    const otpBtn = document.querySelector('button.show-otp-col');
    if (winWidth == 'xl' || winWidth == 'lg' || winWidth == 'md') {
      otpCol.style.maxWidth = maxColWidth;
      otpCol.style.display = 'block';
      otpBtn.style.display = 'none';
    } else if (winWidth == 'sm' || winWidth == 'xs') {
      otpCol.style.maxWidth = 'none';
      otpCol.style.display = 'none';
      otpBtn.style.display = 'block';
    }
  }
}

window.addEventListener('resize', () => {
  const newWinWidth = getWinWidthStr();
  if (winWidth != newWinWidth) {
    winWidth = newWinWidth;
    establishDisplay();
  }
});

const scrollHandler = (event) => {
  if (document.querySelectorAll('#otp-col li.dynamic').length == false) {
    event.target.onscroll = null;
  } else {
    const a = document.createElement('a');
    const arr = document.querySelectorAll('#otp-col a');
    let t = 'on-this-page';
    for (let i = arr.length - 1; i >= 0; i--) {
      a.href = arr[i].href;
      const h = a.hash;
      const hid = h && h.substring(1);
      const el = hid && document.getElementById(hid);
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
    document.getElementById('page-col').scrollTo(0, 0);
  } else {
    const t = document.getElementById(id);
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
  const a = document.querySelector('#otp-col a.active');
  if ( a ) { a.classList.remove('active'); }
  const link =
    (id === 'on-this-page') ?
      document.querySelector('#otp-col a[href="#on-this-page"]') :
      document.querySelector('#otp-col a[href="' + "#" + id + '"]');
  if ( link && link.classList && link.classList.contains('active') === false) {
    link.classList.add('active');
  }
};

const getWebpage = async (folder, hash, shallUpdateHistory) => {
  folder = folder.replace(/index\.html$/, '');
  const url = `${window.location.origin}${folder}`;
  if ( ! folder || ! url ) { throw Error(`getWebpage on undefined`); }

  const pageHtmlP = axiosGetData(`${url}page.html`);
  const otpHtmlP = axiosGetData(`${url}otp.html`);
  const configJson = await axiosGetData(`${url}config.json`);

  // Book or different book?
  if (configJson.bookPath && configJson.bookPath !== currentPage.bookPath) {
    const bookTitle_a = document.getElementById('about-this-book');
    bookTitle_a.href = `/${configJson.bookPath}/`;
    bookTitle_a.innerHTML = configJson.bookTitle;
    let bookHtml = document.createRange().createContextualFragment((await axios.get(`${window.location.origin}/${configJson.bookPath}/book.html`)).data);
    document.querySelectorAll('#book-col div.dynamic').forEach(n => n.remove());
    document.querySelector('#book-col').append(bookHtml);

    // On click chapter-icon.
    document.querySelectorAll('#book-col i.chapter-icon').forEach(el => {
      el.addEventListener('click', (event) => {
        const item = event.target;
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
  const tspan = document.querySelector('div.hh-viewer-wrapper span.title');
  tspan.id = configJson.titleId;
  tspan.textContent = ctitle;
  document.title = `Reach > ${ctitle}`;

  // Update and show/hide edit btn.
  document.getElementById('edit-btn').href = `${github}${folder}index.md`;

  // Write author
  document.querySelector('div.hh-viewer-wrapper span.author').innerHTML = configJson.author ? `By ${configJson.author}` : '';

  // Write published data
  document.querySelector('div.hh-viewer-wrapper span.published-date').innerHTML = configJson.publishedDate ? `Published on ${(new Date(configJson.publishedDate)).toLocaleDateString()}` : '';

  // Write page html
  const pageHtml = await pageHtmlP;
  const pageDoc = document.createRange().createContextualFragment(pageHtml);
  document.querySelector('div.hh-viewer-wrapper div.hh-viewer').textContent = '';
  document.querySelector('div.hh-viewer-wrapper div.hh-viewer').append(pageDoc);

  // If search page.
  const searchInput = document.getElementById('search-input');
  if (searchInput) {
    searchInput.focus();
    searchInput.addEventListener('keyup', function (event) {
      searchIndex.search(searchInput.value).then(({ hits }) => {
        if(hits.length) {
          const searchResultsList = document.getElementById('search-results-list');
          searchResultsList.innerHTML = '';
          hits.forEach((el, index) => {
            const a = document.createElement('a');
            a.href = el.url;
            const anchorTextSpan = document.createElement('span');
            anchorTextSpan.innerHTML = el._highlightResult.title.value;
            a.append(anchorTextSpan);
            const summarySpan = document.createElement('span');
            summarySpan.innerHTML = ` - ${el._highlightResult.summary.value}`;
            const li = document.createElement('li');
            li.append(a);
            li.append(summarySpan);
            searchResultsList.append(li);
          });
        }
      });
    });
  }

  // Write otp html.
  if (configJson.hasOtp) {
    document.querySelectorAll('#otp-col ul ul.dynamic, #otp-col ul li.dynamic').forEach(n => { n.remove(); });
    const otpUl = document.querySelector('#otp-col ul');
    const otpHtml = await otpHtmlP;
    const otpDoc = document.createRange().createContextualFragment(otpHtml);
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
  document.querySelectorAll('a').forEach(el => {
    el.classList.remove('active');
  });

  // Adjust navbar active indicator.
  if (configJson.menuItem) {
    document.getElementById(configJson.menuItem).classList.add('active');
  }

  document.querySelectorAll(`a[href="${folder}"]`).forEach((el) => {
    el.classList.add('active');
  });

  const el = document.querySelector(`#book-col a[href="${folder}"]`);
  if (el) {
    const chapter = el.closest('div.chapter');
    const pages = chapter.querySelector('div.pages');
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
  if (configJson.bookPath) {
    document.getElementById('book-col').classList.remove('banish');
    document.querySelector('div.show-book-col').classList.remove('banish');
  } else {
    document.getElementById('book-col').classList.add('banish');
    document.querySelector('div.show-book-col').classList.add('banish');
  }

  // Display page.
  document.querySelector('div.hh-page-header').style.display = configJson.hasPageHeader ? 'block' : 'none';

  document.getElementById('page-col').style.display = 'block';

  // Display OTP.
  if (configJson.hasOtp) {
    document.getElementById('otp-col').classList.remove('banish');
    document.querySelector('button.show-otp-col').classList.remove('banish');
  } else {
    document.getElementById('otp-col').classList.add('banish');
    document.querySelector('button.show-otp-col').classList.add('banish');
  }

  // Scroll to proper place and update history
  currentPage.folder = folder;
  setClickFollowLink();
  await gotoTarget(shallUpdateHistory, hash ? hash.substring(1) : 'on-this-page');
};

const clickFollowLink = async (evt) => {
  if ( evt.shiftKey || evt.ctrlKey ) { return; }
  const t = evt.target;
  if ( t.classList && t.classList.contains("copyBtn") ) {
    evt.preventDefault();
    await navigator.clipboard.writeText(t.getAttribute('data-clipboard-text'));
    return;
  }
  const href = t.href;
  const a = document.createElement('a');
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
  document.querySelectorAll('a').forEach((el) => {
    el.addEventListener('click', clickFollowLink);
  });
};

window.onpopstate = function (event) {
  const a = document.createElement('a');
  a.href = document.location.href;
  getWebpage(a.pathname, a.hash, false);
};

const makeShowHide = (hideQ, showQ, showId) => {
  const f = (isHide) => {
    const g = (b) => b ? 'block' : 'none';
    const x = g(isHide); const y = g(! isHide);
    const q = isHide ? hideQ : showQ;
    document.querySelector(q).addEventListener('click', (event) => {
      if (winWidth == 'sm' || winWidth == 'xs') {
        document.getElementById('page-col').style.display = x;
      }
      document.getElementById(showId).style.display = y;
      document.querySelector(showQ).style.display = x;
    });
  };
  f(true);
  f(false);
};

makeShowHide('button.hide-book-icon', 'div.show-book-col', 'book-col');
makeShowHide('button.hide-otp-icon', 'button.show-otp-col', 'otp-col');

document.getElementById('page-col').addEventListener('scroll', scrollHandler);

getWebpage(window.location.pathname, window.location.hash, true);

