#lang scribble/manual
@(require "lib.rkt")

@title[
#:version reach-vers
#:tag     "guide-packages"
]{Sharing and discovering shared Reach packages}

Reach makes library packaging very easy: simply push your code to
@link["https://github.com"]{GitHub} or
@link["https://bitbucket.org"]{BitBucket} and
@link["https://git-scm.com/book/en/v2/Git-Basics-Tagging"]{tag it} to mark a
version number.

Packages on GitHub may also activate the
@link["https://github.com/topics/reach-pkg"]{reach-pkg}
topic by configuring it in their repo's
@link["https://docs.github.com/en/github/administering-a-repository/managing-repository-settings/classifying-your-repository-with-topics"]{"About" configuration}
to enjoy greater discoverability.

The @seclink["ref-programs-import-package"]{package imports} reference section
details Reach's remote module import syntax, and
@seclink["ref-usage-compile"] describes the @tt{--install-pkgs} command-line
argument which allows remote packages to be fetched during compilation.
