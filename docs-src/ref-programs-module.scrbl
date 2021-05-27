#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs-module"]{Modules}

A Reach @deftech{source file} is a textual file which specifies a Reach @tech{module}.
It is traditionally given the file extension @litchar{rsh},
e.g. @filepath{dao.rsh}.

A @deftech{module} starts with @reachin{'reach @|reach-short-vers|';}
followed by a sequence of @tech{imports} and @tech{identifier definitions}.
A module can only be compiled or used if it contain one or more @tech{exports}.
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
This function is applied during compilation in as an @tech{application initialization}.
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
a @reachin{applicationArgs} tuple,
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

