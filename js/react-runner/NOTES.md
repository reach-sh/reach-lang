Note: package.json dependencies duplicate the dependencies listed in js-deps/package.json because npm is dumb and `npm install` will delete them since they are not listed.

package.json does NOT include @reachsh/stdlib,
because we do not install this via npm install.
