#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-versions"]{How does Reach use version numbers?}

Reach uses @link["https://semver.org/"]{semantic versioning}, which means that given Reach version number @litchar{MAJOR.MINOR.PATCH},

@itemize[

@item{@litchar{MAJOR} versions are incompatible.}

@item{@litchar{MINOR} versions are compatible, but have additional features relative to earlier versions.}

@item{@litchar{PATCH} versions are entirely compatible.}

]

However, the major version @litchar{0.y.z} is pre-stability and makes no promises about compatibility of any kind.

@(hrule)

Reach source code starts with

@reach{'reach @|reach-short-vers|';}

because this indicates that it relies on this major version and the features added in this minor version.

@(hrule)

Reach tools are specified by the entire version number with a @litchar{v} at the front, @litchar{vMAJOR.MINOR.PATCH}, but are also available at all prefixes (i.e., @litchar{vMAJOR.MINOR} and @litchar{vMAJOR}). Additionally, there is a version @litchar{stable} which resolves to the most recent stable @litchar{vMAJOR} version.
