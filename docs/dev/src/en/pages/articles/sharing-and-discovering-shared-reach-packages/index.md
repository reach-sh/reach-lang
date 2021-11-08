---
author: Matt Audesse
hasOtp: false
menuItem: mi-articles
publishedDate: 2021-06-04T14:00:00
---

# Sharing and discovering shared Reach packages

Reach makes library packaging very easy: simply push your code to [GitHub](https://github.com/) or [BitBucket](https://bitbucket.org/) and [tag it](https://git-scm.com/book/en/v2/Git-Basics-Tagging) to mark a version number.

Packages on GitHub may also activate the [reach-pkg](https://github.com/topics/reach-pkg) topic by configuring it in their repo’s [About](https://docs.github.com/en/github/administering-a-repository/managing-repository-settings/classifying-your-repository-with-topics) configuration to enjoy greater discoverability.

The [package imports](/en/books/essentials/reach-programs/modules/) reference section details Reach’s remote module import syntax, and the [compile]() command reference describes the `–install-pkgs` command-line argument which allows remote packages to be fetched during compilation.