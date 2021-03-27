/* For the Racket manual style */

AddOnLoad(function() {
    /* Look for header elements that have x-source-module and x-part tag.
       For those elements, add a hidden element that explains how to
       link to the section, and set the element's onclick() to display
       the explanation. */
    var tag_names = ["h1", "h2", "h3", "h4", "h5"];
    for (var j = 0; j < tag_names.length; j++) {
        elems = document.getElementsByTagName(tag_names[j]);
        for (var i = 0; i < elems.length; i++) {
            var elem = elems.item(i);
            AddPartTitleOnClick(elem);
        }
    }
})

// cache of source urls
var cache = {};

function ParseSource(source, mod_path, single_collection) {

    var source_url = new URL(source);

    if (source_url.protocol == "github:") {
        // browser URL parser only works with http(s) URLs
        source_url = new URL("https" + source.substring(6));
        var host = source_url.host;
        var url_path = source_url.pathname.substring(1).split("/");
        if (!(url_path.length >= 2)) return null;
        var user = url_path.shift();
        var repo = url_path.shift();
        var branch = url_path.shift();
        var source_path = url_path.join("/");
    }
    else if (("https:" == source_url.protocol) || ("git:" == source_url.protocol)) {
        // browser URL parser only works with http(s) URLs
        if ("git:" == source_url.protocol)
            source_url = new URL("https" + source.substring(3));

        var host = source_url.host;
        var source_path = source_url.searchParams.get("path");
        var branch = (source_url.hash || "#master").substring(1);
        var url_path = source_url.pathname.substring(1).split("/");
        if (url_path.length < 2) throw [source_url.pathname, url_path];
        var user = url_path.shift();
        var repo = url_path.shift();
        var mtch = repo.match(/(.*)\.git$/);
        if (mtch) repo = mtch[1];

    }
    else return null;

    var mod_path_re = /^\(lib "(.+)"\)$/;

    var mod_path_elems = mod_path && mod_path.match(mod_path_re)[1].split("/");

    if (!user || !repo || !mod_path_elems)
        return null;
    if (single_collection)
        mod_path_elems.shift();

    var file_path = mod_path_elems.join("/");


    if (source_path) {
        file_path = source_path + "/" + file_path;
    }

    return { user: user,
             repo: repo,
             file_path: file_path,
             branch: branch,
             host: host };
}

function AddSourceElement(pkg_url, info) {
    info.appendChild(document.createTextNode("Document source "));
    var url_line = document.createElement("div");
    var a = document.createElement("a");
    a.href = pkg_url;
    a.style.whiteSpace = "nowrap";
    a.appendChild(document.createTextNode(pkg_url));
    addSpan(url_line, "\xA0", "RktRdr");
    url_line.appendChild(a);
    info.appendChild(url_line);
}

var prefixes = { "github.com": "tree",
                 "gitlab.com": "-/blob" };


function AddSourceUrl(source, mod_path, collection, info) {
    // multi is encoded as an array, empty as false
    single_collection = (typeof collection === "string");

    var parsed = source && mod_path && ParseSource(source, mod_path, single_collection);

    if (!parsed) return;

    prefix = prefixes.hasOwnProperty(parsed.host) && prefixes[parsed.host];
    if (!prefix) return;

    var correct_url = "https://" + [parsed.host, parsed.user, parsed.repo, prefix, parsed.branch, parsed.file_path].join("/");

    if (info) AddSourceElement(correct_url, info);
}

function addSpan(dest, str, cn) {
    var s = document.createElement("span");
    s.className = cn;
    s.style.whiteSpace = "nowrap";
    s.appendChild(document.createTextNode(str));
    dest.appendChild(s);
}


// test cases
if (false) {
    console.log(ParseSource("git://gitlab.com/benn/foo?path=xxx",
                            '(lib "asn1/scribblings/asn1.scrbl")',
                            false))
    console.log(ParseSource("github://github.com/carl-eastlund/mischief/master",
                            '(lib "asn1/scribblings/asn1.scrbl")',
                            false))
    console.log(ParseSource("github://github.com/carl-eastlund/mischief/stable/dir",
                            '(lib "asn1/scribblings/asn1.scrbl")',
                            false))

    console.log(ParseSource("git://github.com/racket/racket/?path=pkgs/racket-doc",
                            '(lib "asn1/scribblings/asn1.scrbl")',
                            false));

    console.log(ParseSource("git://github.com/rmculpepper/asn1.git?path=asn1-doc",
                            '(lib "asn1/scribblings/asn1.scrbl")',
                            true));
    console.log(ParseSource("git://github.com/rmculpepper/asn1",
                            '(lib "asn1/scribblings/asn1.scrbl")',
                            true));
    console.log(ParseSource("git://github.com/rmculpepper/asn1",
                            '(lib "asn1/scribblings/asn1.scrbl")',
                            false));
}

function AddPartTitleOnClick(elem) {
    var mod_path = elem.getAttribute("x-source-module");
    var tag = elem.getAttribute("x-part-tag");
    var source_pkg = elem.getAttribute("x-source-pkg");

    // create here to share
    var info = document.createElement("div");


    // tag is not needed, but this way we can add the element in only one place
    // avoid failing on browser that don't have `fetch`
    if (mod_path && source_pkg && tag && window.fetch) {

        var cached = cache[mod_path]
        if (cached) {
            AddSourceElement(cached[0], mod_path, cached[1], info);
        }
        else {
            fetch("https://pkgs.racket-lang.org/pkg/" + source_pkg + ".json")
                .then(function (response) { return response.json(); })
                .then(function (data) {
                    var vers = data["versions"] || {};
                    var def = vers["default"] || {};
                    var source = def["source"] || undefined;
                    var collection = data["collection"];
                    if (source) {
                        cache[mod_path] = [source, collection];
                        AddSourceUrl(source, mod_path, collection, info);
                    }
                });
        }
    }

    if (mod_path && tag) {
        // Might not be present:
        var prefixes = elem.getAttribute("x-part-prefixes");

        info.className = "RPartExplain";

        /* The "top" tag refers to a whole document: */
        var is_top = (tag == "\"top\"");
        info.appendChild(document.createTextNode("Link to this "
                                                 + (is_top ? "document" : "section")
                                                 + " with "));

        /* Break `secref` into two lines if the module path and tag
           are long enough: */
        var is_long = (is_top ? false : ((mod_path.length
                                          + tag.length
                                          + (prefixes ? (16 + prefixes.length) : 0))
                                         > 60));

        var line1 = document.createElement("div");
        var line1x = ((is_long && prefixes) ? document.createElement("div") : line1);
        var line2 = (is_long ? document.createElement("div") : line1);

        /* Construct a `secref` call with suitable syntax coloring: */
        addSpan(line1, "\xA0@", "RktRdr");
        addSpan(line1, (is_top ? "other-doc" : "secref"), "RktSym");
        addSpan(line1, "[", "RktPn");
        if (!is_top)
            addSpan(line1, tag, "RktVal");
        if (is_long) {
            /* indent additional lines: */
            if (prefixes)
                addSpan(line1x, "\xA0\xA0\xA0\xA0\xA0\xA0\xA0\xA0", "RktPn");
            addSpan(line2, "\xA0\xA0\xA0\xA0\xA0\xA0\xA0\xA0", "RktPn");
        }
        if (prefixes) {
            addSpan(line1x, " #:tag-prefixes ", "RktPn");
            addSpan(line1x, "'", "RktVal");
            addSpan(line1x, prefixes, "RktVal");
        }
        if (!is_top)
            addSpan(line2, " #:doc ", "RktPn");
        addSpan(line2, "'", "RktVal");
        addSpan(line2, mod_path, "RktVal");
        addSpan(line2, "]", "RktPn");

        info.appendChild(line1);
        if (is_long)
            info.appendChild(line1x);
        if (is_long)
            info.appendChild(line2);

        info.style.display = "none";

        /* Add the new element afterthe header: */
        var n = elem.nextSibling;
        if (n)
            elem.parentNode.insertBefore(info, n);
        else
            elem.parentNode.appendChild(info);

        /* Clicking the header shows the explanation element: */
        elem.onclick = function () {
            if (info.style.display == "none")
                info.style.display = "block";
            else
                info.style.display = "none";
        }
    }
}
