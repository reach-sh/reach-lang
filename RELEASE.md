Release
=======

Some best guesses on a process for a "minor" or "major" version bump of the Reach language.

TODO re: this doc
-----------------

* [ ] Update this doc once we actually do it.
* [ ] Document "patch" version bump process. (Should be simpler.)

Before version bump
-------------------

* Get the repo in its final state for the current version.
* `git tag` with the current version in case we want to branch off of it to backport any critical fixes that may arise.
* Build & push docker images.
* Refresh & upload `js-reach-stdlib` repo.

Do version bump
---------------

* Edit `VERSION` file.
* Edit `reach` shell script.
  * Update `REACH_DEFAULT_VERSION`, `REACH_FULL_VERSION`, and `RV_TAGMIN`.
* Find and replace `reach 0.1` in all committed .rsh files. Find them in places such as:
  * `hs/`
    * `rsh/stdlib.rsh`
    * `test-examples/*/*.rsh`
  * `examples/*/*.rsh`
  * `non-examples/*/*.rsh`
* Edit `Reach/Version.hs`.
* Commit & push to github; observe CI test results.
* Build & push docker images.
* Refresh & upload `js-reach-stdlib` repo.
  * Manually update js lib version prior to upload.
