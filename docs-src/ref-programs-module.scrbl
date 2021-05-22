#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs-module"]{Modules}

A Reach @deftech{source file} is a textual file which specifies a Reach @tech{module}.
It is traditionally given the file extension @litchar{rsh},
e.g. @filepath{dao.rsh}.

A @deftech{module} starts with @reachin{'reach @|reach-short-vers|';}
followed by a sequence of @tech{imports} and @tech{identifier definitions}.
A module can only be compiled or used if it contains one or more @tech{exports}.
@margin-note{See @seclink["guide-versions"]{the guide section on versions} to understand how Reach uses version numbers like this.}

@section[#:tag "ref-programs-module-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for a module.
However, some additional statements are allowed.

@subsection[#:tag "ref-programs-export"]{@tt{export}}

Module-level @tech{identifier definitions} may be @deftech{export}ed
by writing @(mint-define! '("export")) @reachin{export} in front of them.
For example,
@reach{
  export const x = 1;
  export const [a, b, ...more] = [ 0, 1, 2, 3, 4 ];
  export function add1(x) { return x + 1; };
}
are valid @tech{exports}.

Module-level identifiers may also be @tech{export}ed after the fact,
and may be renamed during export. For example:

@reach{
 const w = 2;
 const z = 0;
 export {w, z as zero};
}

Identifiers from other modules may be re-exported (and renamed),
even if they are not imported in the current module.
For example:

@reach{
 export {u, x as other_x} from './other-module.rsh';
}

An @tech{export}ed identifier in a given @tech{module} may be @tech{import}ed by other @tech{modules}.

Exports are also exposed to the frontend via @jsin{getExports}. Functions are only exposed
if they are typed, that is, if they are constructed with @reachin{is}.

@subsection[#:tag "ref-programs-import"]{@tt{import}}

@(mint-define! '("import"))
Reach supports two types of module imports: "local imports", which refer to
modules that exist inside your project, and "package imports", which refer to
remote libraries that may be fetched from external sources such as GitHub.

Package imports are easily distinguished from local imports by a mandatory
@litchar|{@}| character at the beginning of the path string.

@subsubsection{Local imports}
@reach{import 'games-of-chance.rsh';}

When a @tech{module}, @litchar{X}, contains an @deftech{import},
written @reachin{import "LIB.rsh";},
then the path @filepath{LIB.rsh} must resolve to another Reach @tech{source file}.
The @tech{exports} from the @tech{module} defined by @filepath{LIB.rsh} are included in the set of @tech{bound identifier}s in @litchar{X}.

@(mint-define! '("from"))
@reach{import {flipCoin, rollDice as d6} from 'games-of-chance.rsh';}

Import statements may limit or rename the imported @tech{identifiers}.

@reach{import * as gamesOfChance from 'games-of-chance.rsh';}

Imports may instead bind the entire @tech{module} to a single @tech{identifier},
which is an @tech{object} with @tech{fields} corresponding to that @tech{module}'s @tech{exports}.

@tech{Import} cycles are @tech{invalid}.

The path given to an @tech{import} may @bold{not} include @litchar{..} to specify files outside the current directory @bold{nor} may it be an absolute path.

It @bold{must} be a relative path, which is resolved relative to the parent directory of the @tech{source file} in which they appear.

@subsubsection{Package imports}
@reach{
import * as func from
  '@"@"github.com:reach-sh/reach-example-package#main/src/func.rsh';
}

@deftech{Package imports} obey the same rules as local imports but support an
extended path syntax which allows Reach programmers to seamlessly plug into
third-party libraries hosted on the internet.

All package imports begin with the @litchar|{@}| character.

Package import paths are comprised of the following components:
@itemlist[
@item{
@bold{(Optional): The site where the package is hosted.}

Reach currently supports GitHub and BitBucket, and will default to GitHub if no
site is specified.

This component must be followed by a @litchar{:} character.

Possible values include: @tt{github.com:} and @tt{bitbucket.org:}.
}

@item{
@bold{The account registered with the host site.}

This component must be followed by a @litchar{/} character.

Example: @tt{reach-sh/}.
}

@item{
@bold{A repository associated with the account.}

This component must be followed by a @litchar{#} character.

Example: @tt{reach-example-package#}.
}

@item{
@bold{(Optional): A @tt{git ref} or @tt{git commit} used to represent the
package version.}

Since @tt{git} repositories evolve and change over time, Reach must take extra
steps in order to pin a given module import's version in its @tech{lockfile}.

If no @tt{ref} is specified, Reach first tries to find the requested module
on the repository's @tt{master} branch, and if that fails then on the @tt{main}
branch once more.
@margin-note{
@tt{git refs} are discussed in further detail
@link["https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddefrefaref"]{here}.
}

If @tt{master} @italic{is} specified but the requested module does not exist on
the @tt{master} branch, then Reach will again attempt to find it on @tt{main}.

In either case, if the module cannot be found on either @tt{master} or
@tt{main}, then Reach will emit a failure message during compilation.

@bold{It is highly advisable that package authors use @tt{git tags} to denote
version "releases", e.g. @tt{v0.2.1}, and that consuming code target the
desired @tt{git tag} rather than a branch name.}
@margin-note{
Read
@link["https://git-scm.com/book/en/v2/Git-Basics-Tagging"]{this guide}
to learn more about how @tt{git tags} work.
}

Reach ultimately uses @tt{git rev-parse} to pin with @tt{SHA} hashes (and is
therefore resilient to @tt{git tags} which are later modified to point at
another commit), but following this strategy will improve program semantics for
both package authors and consumers.

Example: @tt{v3.0.6}.
}

@item{
@bold{(Optional): The directory in which the requested module may be found.}

This component must be preceded by a @litchar{/} character.

Example: @tt{/src/lib}.
}

@item{
@bold{(Optional): The filename of the requested module.}

Defaults to @tt{index.rsh}.

This component must be preceded by a @litchar{/} character.

Example: @tt{/pkg.rsh}.
}
]

@section[#:tag "ref-programs-module-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-compute-exprs"]{computation} are valid for a module.
However, some additional expressions are allowed.

@subsection[#:tag "ref-programs-reach.app"]{@tt{Reach.App}}

@(mint-define! '("Reach") '("App"))
@reach{
export const main = Reach.App(() => {
 const A = Participant("A", {
  displayResult: Fun(Int, Null),
 });
 deploy();

 const result = 0;
 A.only(() => { interact.displayResult(result); });

 exit();
});
}

@deftech{Reach.App} accepts a no-argument function that specifies a @|DApp|.
This function is applied during compilation as an @tech{application initialization}.
It specifies the entire @|DApp| in its body.

If the result of @reachin{Reach.App} is eventually bound to an identifier that is @tech{export}ed, then that identifier may be a target given to the compiler, as discussed in @seclink["ref-usage-compile"]{the section on usage}.

@subsubsection{Deprecated long-form}

@reach{
export const main =
  Reach.App({}, [Participant("A", {displayResult: Fun(Int, Null)})], (A) => {
    const result = 0;
    A.only(() => { interact.displayResult(result); })
    exit();
  });
}

Previous versions of Reach only allowed a form of @tech{Reach.App} which accepted three arguments:
an @reachin{options} object,
an @reachin{applicationArgs} tuple,
and a @reachin{program} arrow of the form @reachin{(applicationIds) => body}.

This form was equivalent to
@reach{
Reach.App(() => {
 setOptions(options);
 [ applicationIds ] = applicationArgs;
 deploy();
 body
});
}

The current version of Reach will automatically transform these "ternary" @reachin{Reach.App} instances into the above form.

Future versions of Reach will deprecate this transform and such programs will be @tech{invalid}.

