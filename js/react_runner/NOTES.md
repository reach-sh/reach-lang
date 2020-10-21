Note: package.json dependencies duplicate the dependencies listed in js_deps/package.json because npm is dumb and `npm install` will delete them since they are not listed.

It would be preferable to remove these deps and the "optional-dependencies" section w/ ganache-core if possible.
