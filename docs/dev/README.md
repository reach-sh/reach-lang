# Reach Developer Portal

This repository contains the source files and site generator for the [Reach Developer Portal](https://docs.reach.sh/en/books/essentials/).

## Deploy the site locally

1. Fork the [reach-lang](https://github.com/reach-sh/reach-lang) repository to your Github account.

1. Clone your fork to your computer. Here is an example:

    ```
    git clone https://github.com/my-account/reach-lang.git
    ```

1. Change directory and convert source files to website files:

    ```
    cd docs
    make build
    ```

    Initially, this build will take 7-8 minutes.
    Subsequent runs will take on the order of 30 seconds.

1. Run a server:

    ```
    make serve-up
    ```

   You can turn it off with `make serve-down`.

1. Browse to the site:

    * http://localhost:8080 is the original docs site.
    * http://localhost:8080/en/books/essentials is the new RDP site.

1. Rebuild after making changes to one or more RDP source files:

    ```
    cd docs
    make build
    ```

    This build will take about 40 seconds. You do not have to restart your server.

## Create a webpage

1. Create a page folder (e.g. colors-and-shapes):

    ```
    cd docs/dev
    mkdir -p src/en/pages/colors-and-shapes
    ```

1. Create an index.md file inside your page folder:

    ```
    touch src/en/pages/colors-and-shapes/index.md
    ```

1. Add content to the index.md file. For sample content, browse to the [Sample Page](https://docs.reach.sh/en/pages/sample/), click the Pencil icon, click the second Pencil icon, and copy & paste.

1. Generate the webpage:

    ```
    cd docs
    make build
    ```

1. Browse to your new page.

## Configure the webpage

Consider the following index.md file:

 ```
 # Colors and Shapes

Lorem ipsum dolor sit amet ...
```

Because this file specifies no frontmatter, the corresponding webpage reflects default options:

<p><img src="./assets/page-options-defaults.png" width=800></p>

Adding frontmatter to an index.md file changes the presentation and/or behavior of the corresponding webpage. Note the added frontmatter:

```
---
author: Sarah Smiles
hasOtp: false
hasPageScrollbar: false
menuItem: mi-docs
publishedDate: 2021-09-30T14:00:00
---

# Colors and Shapes

Lorem ipsum dolor sit amet ...
```

The corresponding webpage reflects the newly specified options:

<p><img src="./assets/page-options-set.png" width=800></p>

Below is a table of the current page configuration options:

|Option|Type|Default|Description|
|-|-|-|-|
|author|string|null|Displays "By" + author.|
|hasEditBtn|boolean|true|Displays or hides the pencil icon.|
|hasOtp|boolean|true|Displays or hides the *On This Page* panel.|
|hasPageHeader|boolean|true|Displays or hides the title, icons, author, and publication date.|
|hasPageScrollbar|boolean|true|Displays or hides the page scrollbar. Scrolling works either way.|
|hasRefreshBtn|boolean|true|Displays or hides the refresh icon.|
|menuItem|string|null|Internal use at this point.|
|publishedDate|string|null|Displays or hides the publication date. Use 2021-09-30T14:00:00 GMT format.|

## About source files

Each webpage traces its source to a folder within the [books](https://github.com/reach-sh/reach-lang/tree/master/docs/dev/src/en/books) or [pages](https://github.com/reach-sh/reach-lang/tree/master/docs/dev/src/en/pages) directories.

<p><img src="./assets/folder-to-webpage.png" width=400></p>

The [books](https://github.com/reach-sh/reach-lang/tree/master/docs/dev/src/en/books) directory contains book, chapter, and leaf folders which correspond to book, chapter, and leaf webpages:

<p><img src="./assets/books.png" width=700></p>

The [pages](https://github.com/reach-sh/reach-lang/tree/master/docs/dev/src/en/pages) directory contains standalone and dummy folders. Standalone folders correspond to webpages. Dummy folders do not correspond to webpages. Instead, they provide a user-determined organizational hierarchy for standalone folders.

<p><img src="./assets/pages.png" width=700></p>

Each page folder (book, chapter, leaf, standalone) houses an index.md file containing the source for the webpage.

<p><img src="./assets/page-folder.png" width=300></p>

The index.md file conforms to [Github-flavored markdown](https://github.github.com/gfm/). It often begins with a hash symbol + space + page title:

```
# Demo Page
```

It may contain headings, paragraphs, lists, code snippets, tables, html, etc. as demonstrated on the [Demo Page](https://github.com/reach-sh/reach-lang/blob/master/docs/dev/src/en/pages/demo/index.md). It may also contain links to supplemental files (e.g. images) that reside in the same folder, and links to external resources (e.g. videos):

<p><img src="./assets/supplemental-files.png" width=600></p>

## Responsive Defaults

The various screen widths (seen below) determine whether a user sees, on initial load, a book column versus an expander hamburger (on the left) and an on-this-page (otp) column versus an expander hamburger (on the right).

> **Important**<br/>
> The user explicitly expanding or collapsing the OTP column sets a "preferred state" flag which the application applies to every medium, large, or extra large page the user visits. Reloading the JavaScript file clears this flag. I could save this state to localStorage, but I don't see an immediate need. 

#### Extra Large

<p><img src="./assets/w-xl.png" width=800></p>

#### Large

<p><img src="./assets/w-lg.png" width=630></p>

#### Medium

<p><img src="./assets/w-md.png" width=530></p>

#### Small

<p><img src="./assets/w-sm.png" width=410></p>

#### Extra Small

<p><img src="./assets/w-xs.png" width=280></p>

## Themes

Reach colors are reddish (#F45747), light blueish (#6AC6E7), and inkish (#1A1C23).

```
background: "url(/src/en/pages/home/stars2.jpeg), rgba(var(--bs-dark-rgb),var(--bs-bg-opacity))"
```

## About the generator

The site generator is implemented with Node.js and has no options.
It builds the site all at once.
It generally takes about 25 seconds to build.

```
$ node generator.js
```
