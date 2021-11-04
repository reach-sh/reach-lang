---
menuItem: mi-docs
---

# Demo Page

This page helps you compare rendered html to source markdown. To view the Github Flavored Markdown (GFM) source file for this page, click the pencil icon above which opens a new browser tab. Then, on the new tab, click the "Edit this file" pencil icon. Now, you can compare what you see on this page to what the page author wrote in the source markdown.

# Code Snippets

Code snippets have the following boolean characteristics:

* Numbered (Num). 
* Highlighted (HL) by language.
* Loaded from some other file.
* Ranged which applies only to loaded snippets.

Here are the possiblities:

|Name|Highlighted|Numbered|Loaded|Ranged|
|-| :-: | :-: | :-: | :-: |
|HL Num Loaded Ranged|✓|✓|✓|✓|
|HL Num Loaded|✓|✓|✓|&nbsp;|
|HL Num |✓|✓|&nbsp;|&nbsp;|
|HL Loaded Ranged|✓|&nbsp;|✓|✓|
|HL Loaded|✓|&nbsp;|✓|&nbsp;|
|HL|✓|&nbsp;|&nbsp;|&nbsp;|
|Num Loaded Ranged|&nbsp;|✓|✓|✓|
|Num Loaded|&nbsp;|✓|✓|&nbsp;|
|Num|&nbsp;|✓|&nbsp;|&nbsp;|
|Loaded Ranged|&nbsp;|&nbsp;|✓|✓|
|Loaded|&nbsp;|&nbsp;|✓|&nbsp;|
|Simple|&nbsp;|&nbsp;|&nbsp;|

## HL Num Loaded Ranged

``` js
load: /examples/tut-3/index.rsh
range: 3-6
```

## HL Num Loaded

``` js
load: /examples/tut-3/index.rsh
```

``` js
load: /examples/tut-9/views/AppViews.js
```

## HL Num

``` js
const Player = {
  getHand: Fun([], UInt),
  seeOutcome: Fun([UInt], Null),
};
```

``` bash
$ make build
reach compile index.rsh
Verifying knowledge assertions
Verifying for generic connector
  Verifying when ALL participants are honest
  Verifying when NO participants are honest
  Verifying when ONLY "Buyer" is honest
  Verifying when ONLY "Seller" is honest
Checked 34 theorems; No failures!
docker build -f Dockerfile --tag=reachsh/reach-app-market-day:latest .
```

## HL Loaded Ranged

``` js unnumbered
load: /examples/tut-3/index.rsh
range: 3-6
```

## HL Loaded

``` js unnumbered
load: /examples/tut-3/index.rsh
```

## HL

``` js unnumbered
const Player = {
  getHand: Fun([], UInt),
  seeOutcome: Fun([UInt], Null),
};
```

## Num Loaded Ranged

```
load: /examples/tut-3/index.rsh
range: 3-6
```

## Num Loaded

```
load: /examples/tut-3/index.rsh
```

## Num

```
const Player = {
  getHand: Fun([], UInt),
  seeOutcome: Fun([UInt], Null),
};
```

## Loaded Ranged

``` unnumbered
load: /examples/tut-3/index.rsh
range: 3-6
```

## Loaded

``` unnumbered
load: /examples/tut-3/index.rsh
```

## Simple

``` unnumbered
const Player = {
  getHand: Fun([], UInt),
  seeOutcome: Fun([UInt], Null),
};
```

# Paragraphs

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur

# Headings

This heading reflects an `H1` element. The site uses `H1`, `H2`, and `H3` elements on content pages. Only `H1` and `H2` headings appear on the *On This Page* panel.

## Subheadings

This heading reflects an `H2` element. 

### Heading 3

This heading reflects an `H3` element.

# Links

* [Internal Page](/en/pages/articles/)
* [External Page](https://reach.sh/)

# Tables

Tables look like this:

||1|2|3|4|5|
|-|-|-|-|-|-|
|English|One|Two|Three|Four|Five|
|Français|Un|Deux|Trois|Quatre|Cinq|
|日本語|一|二|三|四|五|
|Deutsche|Ein|Zwei|Drei|Vier|Funf|

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

# Images

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

## Responsive

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

<p><img src="img-800.png" class="img-fluid mx-auto d-block" width=700 loading="lazy"></p>

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

## Left-justified

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

<p><img src="img-800.png" width="500" loading="lazy"></p>

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

## Centered-justified

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

<p><img src="img-800.png" width="500" class="mx-auto d-block" loading="lazy"></p>

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

## Right-justified

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

<p class="text-end"><img src="img-800.png" width="500" loading="lazy"></p>

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

## Indented

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

<p><img src="img-800.png" width="500" class="hh-halign-indent" loading="lazy"></p>

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

# Videos

You can [link to a video](https://youtu.be/HCT-FurFVLQ) or embed a video. Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

<p class="ratio ratio-16x9 mx-auto" style="max-width:600px;">
  <iframe 
    src="https://www.youtube.com/embed/HCT-FurFVLQ" 
    frameborder="0"  
    allowfullscreen>
  </iframe>
</p>

Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

# Lists

Technical documentation often uses various types of lists.

## Compact ordered

1. Blue bell
1. Blue jeans
1. Blue yonder

## Compact unordered

* Blue bell
* Blue jeans
* Blue yonder

## Spaced ordered

1. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

    Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nam libero tempore cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est omnis dolor repellendus.

1. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

## Spaced unordered

* Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

    Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nam libero tempore cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est omnis dolor repellendus.

* Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

* Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

## Compact nested ordered

1. Leaves of grass.
1. Leaves of grass.
    1. Leaves of grass.
    1. Leaves of grass.
        1. Leaves of grass.
        1. Leaves of grass.
            1. Leaves of grass.
            1. Leaves of grass.
1. Leaves of grass.
1. Leaves of grass.

## Compact nested unordered

* Leaves of grass.
* Leaves of grass.
    * Leaves of grass.
    * Leaves of grass.
        * Leaves of grass.
        * Leaves of grass.
            * Leaves of grass.
            * Leaves of grass.
* Leaves of grass.
* Leaves of grass.

## Roomy nested ordered

1. Leaves of grass.

1. Leaves of grass.

    1. Leaves of grass.
    1. Leaves of grass.
        1. Leaves of grass.
        1. Leaves of grass.
            1. Leaves of grass.
            1. Leaves of grass.

1. Leaves of grass.

1. Leaves of grass.

## Roomy nested unordered

* Leaves of grass.

* Leaves of grass.

    * Leaves of grass.
    * Leaves of grass.
        * Leaves of grass.
        * Leaves of grass.
            * Leaves of grass.
            * Leaves of grass.

* Leaves of grass.

* Leaves of grass.

## Spaced nested ordered

1. Leaves of grass.

1. Leaves of grass.

    1. Leaves of grass.

    1. Leaves of grass.

        1. Leaves of grass.

        1. Leaves of grass.

            1. Leaves of grass.

            1. Leaves of grass.

1. Leaves of grass.

1. Leaves of grass.

## Spaced nested unordered

* Leaves of grass.

* Leaves of grass.

    * Leaves of grass.

    * Leaves of grass.

        * Leaves of grass.

        * Leaves of grass.

            * Leaves of grass.

            * Leaves of grass.

* Leaves of grass.

* Leaves of grass.

## Multi-element

1. Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

    ```
    $(function () {
      base.winWidth = base.calcWinWidthStr()
      base.currentPage.baseId = $('meta[name='base-id']').attr('content')
      getPage(pathnameToIdentifier(window.location.pathname))
    })
    ```

1. Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

    ```
    load: /examples/tut-6/index.rsh
    ```

1. Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

    <table>
      <thead>
        <tr><th>&nbsp;</th><th>1</th><th>2</th><th>3</th><th>4</th><th>5</th></tr>
      </thead>
      <tbody>
        <tr><td>English</td><td>One</td><td>Two</td><td>Three</td><td>Four</td><td>Five</td></tr>
        <tr><td>Français</td><td>Un</td><td>Deux</td><td>Trois</td><td>Quatre</td><td>Cinq</td></tr>
        <tr><td>日本語</td><td>一</td><td>二</td><td>三</td><td>四</td><td>五</td></tr>
        <tr><td>Deutsche</td><td>Ein</td><td>Zwei</td><td>Drei</td><td>Vier</td><td>Funf</td></tr>
      </tbody>
    </table>

1. Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

    <img src="img-800.png" width="200" height="120" loading="lazy">

1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    1. Blue bell
    1. Blue jeans
    1. Blue yonder

1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    * Blue bell
    * Blue jeans
    * Blue yonder

1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    1. Red

    1. Yellow

    1. Blue

    1. Orange

1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    * Red

    * Yellow

    * Blue

    * Orange

1. Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

    1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

1. Lorem ipsum dolor sit amete, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

    * Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    * Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. 

    Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur.

1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    ```
    $(function () {
      base.winWidth = base.calcWinWidthStr()
      base.currentPage.baseId = $('meta[name='base-id']').attr('content')
      getPage(pathnameToIdentifier(window.location.pathname))
    })
    ```

    Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    <table>
      <thead>
        <tr><th>&nbsp;</th><th>1</th><th>2</th><th>3</th><th>4</th><th>5</th></tr>
      </thead>
      <tbody>
        <tr><td>English</td><td>One</td><td>Two</td><td>Three</td><td>Four</td><td>Five</td></tr>
        <tr><td>Français</td><td>Un</td><td>Deux</td><td>Trois</td><td>Quatre</td><td>Cinq</td></tr>
        <tr><td>日本語</td><td>一</td><td>二</td><td>三</td><td>四</td><td>五</td></tr>
        <tr><td>Deutsche</td><td>Ein</td><td>Zwei</td><td>Drei</td><td>Vier</td><td>Funf</td></tr>
      </tbody>
    </table>

    Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

    <img src="img-800.png" width="200" height="120" loading="lazy">

    Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

1. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.

# Inline text

Authors can use the following inline markdown:

|Markdown|HTML|
|-|-|
|`*snowman*`|`<em>snowman</em>`|
|`_snowman_`|`<em>snowman</em>`|
|`**snowman**`|`<strong>snowman</strong>`|
|`__snowman__`|`<strong>snowman</strong>`|
|`` `snowman` ``|`<code>snowman</code>`|
|`~~snowman~~`|`<del>snowman</del>`|
|`> one` <br> `> two`|`<blockquote><p>one two</p></blockquote>`|
|`https://github.com`|`<a href="https://github.com">https://github.com</a>`|
|`[Github](https://github.com)`|`<a href="https://github.com">Github</a>`|
|`![logo](pic.png)`|`<img src="pic.png"`|

# HTML

## HTML Table

<table>
  <thead>
    <tr><th>&nbsp;</th><th>1</th><th>2</th><th>3</th><th>4</th><th>5</th></tr>
  </thead>
  <tbody>
    <tr><td>English</td><td>One</td><td>Two</td><td>Three</td><td>Four</td><td>Five</td></tr>
    <tr><td>Français</td><td>Un</td><td>Deux</td><td>Trois</td><td>Quatre</td><td>Cinq</td></tr>
    <tr><td>日本語</td><td>一</td><td>二</td><td>三</td><td>四</td><td>五</td></tr>
    <tr><td>Deutsche</td><td>Ein</td><td>Zwei</td><td>Drei</td><td>Vier</td><td>Funf</td></tr>
  </tbody>
</table>
