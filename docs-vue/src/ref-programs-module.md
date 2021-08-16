



# {#ref-programs-module} Modules

A Reach <Defn :name="source file">source file</Defn> is a textual file which specifies a Reach module.
It is traditionally given the file extension `rsh`,
e.g. `dao.rsh`.

A <Defn :name="module">module</Defn> starts with `'reach reach-short-vers';`
followed by a sequence of imports and identifier definitions.
::: note
See [the guide section on versions](##guide-versions) to understand how Reach uses version numbers like this.
:::

## {#ref-programs-module-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for a module.
However, some additional statements are allowed.

### {#ref-programs-export} `export`

Module-level identifier definitions may be <Defn :name="export">export</Defn>ed
by writing <Ref :name="(quote rsh):export" /> `export` in front of them.
For example,
```reach
export const x = 1;
export const [a, b, ...more] = [ 0, 1, 2, 3, 4 ];
export function add1(x) { return x + 1; };
```

are valid exports.

Module-level identifiers may also be exported after the fact,
and may be renamed during export. For example:

```reach
const w = 2;
const z = 0;
export {w, z as zero};
```


Identifiers from other modules may be re-exported (and renamed),
even if they are not imported in the current module.
For example:

```reach
export {u, x as other_x} from './other-module.rsh';
```


An exported identifier in a given module may be imported by other modules.

Exports are also exposed to the frontend via `getExports`. Functions are only exposed
if they are typed, that is, if they are constructed with `is`.

### {#ref-programs-import} `import`

<Ref :name="(quote rsh):import" />

Reach supports two types of module <Defn :name="imports">imports</Defn>: local imports,
which refer to modules that exist within your project, and
package imports, which refer to remote libraries that may be fetched
from external sources such as XXX link-github.
::: note
Read [the guide section on packages](##guide-packages) for more details.
:::

Package imports are easily distinguished from local imports by a
mandatory `@` character at the beginning of the path string.

#### {#ref-programs-import-local} Local imports
```reach
import 'games-of-chance.rsh';
```


When a module, `X`, contains a <Defn :name="local import">local import</Defn>,
written `import "LIB.rsh";`,
then the path `LIB.rsh` must resolve to another Reach source file.
The exports from the module defined by `LIB.rsh` are included in the set of bound identifiers in `X`.

<Ref :name="(quote rsh):from" />
```reach
import {flipCoin, rollDice as d6} from 'games-of-chance.rsh';
```


Import statements may limit or rename the imported identifiers.

```reach
import * as gamesOfChance from 'games-of-chance.rsh';
```


Imports may instead bind the entire module to a single identifier,
which is an object with fields corresponding to that module's exports.

Import cycles are invalid.

The path given to an import may **not** include `..` to specify files outside the current directory **nor** may it be an absolute path.

It **must** be a relative path, which is resolved relative to the parent directory of the source file in which they appear.

#### {#ref-programs-import-package} Package imports
```reach
import * as func from
  '@reach-sh/reach-example-package';
import * as func from
  '@reach-sh/reach-example-package:src/func.rsh';
import * as func from
  '@github.com:reach-sh/reach-example-package#main:src/func.rsh';
```


<Defn :name="Package imports">Package imports</Defn> obey the same rules as local imports but
support an extended path syntax which allows Reach programmers to seamlessly
plug into third-party libraries hosted on the internet.

All package imports begin with the `@` character.

Package import paths are comprised of the following components:
+ **(Optional): The `git` server where the package is hosted.**

This component must be followed by a `:` character.

This component defaults to XXX link-github (i.e. `github.com`) if no site is specified.

Examples: `github.com:`, `bitbucket.org:`.
+ **The account registered with the host site.**

This component must be followed by a `/` character.

Examples: `reach-sh/`, `jeapostrophe/`.
+ **A repository associated with the account.**

Examples: `reach-example-package`, `nfts`.
+ **(Optional): A `git ref` or `git commit` used to represent the
package version.**

If no `ref` is specified, Reach first tries to find the requested module
on the repository's `master` branch, and if that fails then on the `main`
branch once more.
::: note
`git refs` are discussed in further detail
[here](https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddefrefaref).
:::

**It is highly advisable that package authors use `git tags` to denote
version "releases", e.g. `v0.2.1`, and that consuming code target the
desired `git tag` rather than a branch name.**
::: note
Read
[this guide](https://git-scm.com/book/en/v2/Git-Basics-Tagging)
to learn more about how `git tags` work.
:::

This component must be preceded by a `#` character.

Example: `#v3.0.6`.
+ **(Optional): The directory in which the module may be found.**

This component must be preceded by a `:` character and must end with
a `/`.

Example: `:src/lib/`.
+ **(Optional): The filename of the requested module.**

Defaults to `index.rsh`.

If the module exists within a subdirectory it must be preceded by a `/`
character.

Example: `@reach-sh/example#v1.01:parent/child/pkg.rsh`.

However, if the module is stored in the root of the repository, it must instead
be preceded by a `:` character.

Example: `@reach-sh/example#v1.02:pkg.rsh`.


The following forms are all syntactically valid package import
expressions:

XXX (pkg-fmts)

---

Since `git` repositories evolve and change over time, Reach takes extra
steps in order to pin a given module import's version to the specific `SHA` hash of the specified revision at the time the package is first installed.
These pins are stored in a lockfile, which should be included in your source control system.

## {#ref-programs-module-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-compute-exprs) are valid for a module.
However, some additional expressions are allowed.

### {#ref-programs-reach.app} `Reach.App`

<Ref :name="(quote rsh):Reach" /><Ref :name="(quote rsh):App" />
```reach
export const main = Reach.App(() => {
 const A = Participant("A", {
  displayResult: Fun(Int, Null),
 });
 deploy();

 const result = 0;
 A.only(() => { interact.displayResult(result); });

 exit();
});
```


<Defn :name="Reach.App">Reach.App</Defn> accepts a no-argument function that specifies a DApp.
This function is applied during compilation as an application initialization.
It specifies the entire DApp in its body.

If the result of `Reach.App` is eventually bound to an identifier that is exported, then that identifier may be a target given to the compiler, as discussed in [the section on usage](##ref-usage-compile).

#### Deprecated long-form

```reach
export const main =
  Reach.App({}, [Participant("A", {displayResult: Fun(Int, Null)})], (A) => {
    const result = 0;
    A.only(() => { interact.displayResult(result); })
    exit();
  });
```


Previous versions of Reach only allowed a form of Reach.App which accepted three arguments:
an `options` object,
an `applicationArgs` tuple,
and a `program` arrow of the form `(applicationIds) => body`.

This form was equivalent to
```reach
Reach.App(() => {
 setOptions(options);
 [ applicationIds ] = applicationArgs;
 deploy();
 body
});
```


The current version of Reach will automatically transform these "ternary" `Reach.App` instances into the above form.

Future versions of Reach will deprecate this transform and such programs will be invalid.

