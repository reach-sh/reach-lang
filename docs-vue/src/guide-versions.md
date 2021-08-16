


# {#guide-versions} How does Reach use version numbers?

Reach uses [semantic versioning](https://semver.org/), which means that given Reach version number `MAJOR.MINOR.PATCH`,

+ `MAJOR` versions are incompatible.
+ `MINOR` versions are compatible, but have additional features relative to earlier version.
+ `PATCH` versions are entirely compatible.


However, the major version `0.y.z` is pre-stability and makes no promises about compatibility of any kind.

---

Reach source code starts with

```reach
'reach reach-short-vers';
```


because this indicates that it relies on this major version and the features added in this minor version.

---

Reach tools are specified by the entire version number with a `v` at the front, `vMAJOR.MINOR.PATCH`, but are also available at all prefixes (i.e., `vMAJOR.MINOR` and `vMAJOR`). Additionally, there is a version `stable` which resolves to the most recent stable `vMAJOR` version.
