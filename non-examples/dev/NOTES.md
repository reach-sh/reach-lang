# example list

- [x] `race.rsh` --- `race`, no timeout, no variables
- [x] `fork.rsh` --- `fork`, no timeout
- [x] `chicken-fork.rsh` --- `fork`, timeout
- [x] `chicken-race.rsh` --- `race`, timeout
- [ ] --- `race`, variables
- [x] `chicken-parallel.rsh` --- `parallel_reduce`, no class
- [x] `popularity-contest.rsh` --- `parallel_reduce`, class
- [x] `popularity-contest-forin.rsh` --- `parallel_reduce`, `for ... in` syntax
- [ ] --- containers, `Map`
- [ ] --- containers, `Set`
- [ ] --- containers, `Stack`
- [ ] --- containers, assignment syntax
- [x] `raffle.rsh` --- `parallel_reduce`, class, containers, `Map`, `fork`

Explanation of programs
- `{race, fork}.rsh` --- Alice and Bob compete to be the first to send the
  third message to win the prize.
- `chicken-*.rsh` --- Alice and Bob try to out do each other and be the last
  one to pay the gas price before the deadline.
- `raffle.rsh` --- A raffle with two rounds: one to buy a spot and provide
  randomness and a second to reveal the randomness.
- `popularity-contest*.rsh` --- A pollster proposes two candidates, who
  receives votes; the winner receives all the proceeds and the loser gets
  none.

# race --- 1-continuation asymmetric non-determinism

```
// step
const x =
  race([[Part, Thunk],
        ...])
  .timeout();
// consensus-step
invariant(....);
....;
commit();
```

where each `Thunk` must be an `only` (local step) of the participant and then a `publish` & `pay` (but not `timeout`), which must be used once. The thunk
has to return a value, which is unified with all the other thunks. There is
also a `fail` function that can be called by the `only` to cancel the race.
Furthermore, the publication can use `failed` to run code on a failure.

The `fail` function is implicitly called if any `assume` fails.

Change `fail` to `cancel`?

Each participant must be bound before running the race.

The bindings local to a participant are lifted to a `Maybe` of the value, where
the value is `Some` if it ran and `None` if it didn't run. The `Thunk` is not
really a thunk though, because you can write variables in it and if they are
`None` then the branch won't run.

# fork --- n-continuation asymmetric non-determinism

```
// step
fork([[Part, Thunk], ...])
  .timeout();
<empty>
```

The inner thunks are like the other race version.

Each participant may not be bound before running the race.

Potentially this could just be a certain pattern of `race`.

# parallel_reduce --- 1-continuation symmetric non-determinism

```
// step
const x =
  parallel_reduce(
    deadline,
    initial,
    invariant(....),
    [[Part or Class, Thunk],
      ...]);
// step
```

The thunks and invariant have `x` in scope.

The inner thunks are like the other race versions, returning a new value for
`x`.

Participants that are not classes must be bound before the race.

I'm considering allowing an abbrevation of the form:

```
var x = initial;
deadline(....);
invariant(....);
for ( const id in Class ) {
  // Thunk body referencing id rather than Class
}
```

for when there's just one element in the `join`.

# containers

```
const m = new Map(Type)
m[addr] :: Maybe Type
m.set(addr, val :: Type) :: Map(Type)
m.del(addr)
// Must be used affinely
```

```
Set = Map(Null)
s[addr] :: Bool
s.add(addr) :: Set
s.del(addr) :: Set
// Must be used affinely
```

```
Stack(A) = [ UInt, Mapping(UInt, A) ]
s[i] :: Maybe A
s.length :: UInt
s.push(a) :: [ UInt, Stack(A) ]
s.pop() :: Maybe (A, Stack(A))
// Must be used affinely
```

It might be necessary for containers to specify an invariant on their values.

Maybe add queues and deques.

Affinely means that a value can only be "consumed" at most once, where
"consumed" means modified, like by `set`/`del`/`push`/`pop`/etc, and cannot be
"observed" after being "consumed", where "observed" means an operation like
`[]` or `.length`.

---

Maybe we'll break with normal Reach rules and allow redefining of a variable if it
assigned to its own linear copy:
```
const m = new Map(Type);
const m = m.set(addr, val);
m[addr]
```
And maybe we'll support `let` and mutation notation:
```
let m = new Map(Type);
m = m.set(addr, val);
m[addr]
```
But this whole thing might be dangerous with capturing references (like how
`isWinner` in `raffle-foreach.rsh` captures `ticketsM`).
